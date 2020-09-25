############################################################
## Discovering words trends on Afnic domains
############################################################

## Load libraries #####
pacman::p_load(data.table, 
               lubridate, 
               ggplot2, 
               dplyr, 
               wordsplitter, ## This is my own library, not in CRAN!!
               here,
               ggtext, 
               broom, 
               future, 
               future.apply)

## Load graph theme ####
theme_words <- function(...) {
  theme_minimal() +
    theme(
          plot.title = element_markdown(lineheight = 1.1),
          plot.subtitle = element_markdown(lineheight = 1.1),
          legend.position = "none",
          text = element_text(family = "Ubuntu Regular", color = "#22211d"),
          strip.text.x = element_text(size = 10, color = "#4e4d47", face = "bold"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          ...)
}

## Load future type ####
plan("sequential")

## Load data #####
if(!exists("afnic_dt")) {
  source(here( "scripts", "read_afnic.R"))
}


# Stop words
stop_words_list <- unlist(lapply(c("en", "fr"), stopwords::stopwords))

## load functions ####
splitnames_function <- function(dt) {
  
  message(Sys.time(), "   Prepare data: Load wordsplitter dictionaries")
  wordsplitter::setLangDict(lang = "fr", frDic)
  wordsplitter::setLangDict(lang = "en", enDic)
  message(Sys.time(), "   Success preparing data: Load wordsplitter dictionaries")
  
  
  message(Sys.time(), "   Prepare data: Split SLDs in words")
  dt$split <- parallel::mclapply(dt$domain_sld, 
                                 wordsplitter::wordsplitterplus,
                                 lang = c("fr", "en"), 
                                 mc.cores = parallel::detectCores() - 1,
                                 mc.cleanup = TRUE)
  message(Sys.time(), "   Success preparing data: Split SLDs in words. #rows: ", format(nrow(dt), big.mark=","))
  
  message(Sys.time(), "   Prepare data: One row per word")
  dt_splited <- dt[, .(sld_word = unlist(split)), by = setdiff(names(dt), "split")]
  message(Sys.time(), "   Success preparing data: One row per word. #rows: ", format(nrow(dt_splited), big.mark=","))
  
  return(dt_splited)
  
}

## Data preparation #####

# Select relevant columns
domain_date_dt <- afnic_dt[registration_date > as.Date("2019-01-01"), .(domain_name, registration_date)]

# Add weeek and month day
domain_date_dt[, week_date := floor_date(registration_date, unit = "week", week_start = 1)]
domain_date_dt[, month_date := floor_date(registration_date, unit = "month")]

# Add sld
domain_date_dt[, domain_sld := sub(x = domain_name, 
                                   pattern = "(^[0-9a-z-]+)(\\..*)$", 
                                   replacement = "\\1")]


# Split sld
domain_date_prep_dt <- splitnames_function(domain_date_dt)

domain_date_prep_dt <- domain_date_prep_dt[, word_length := nchar(sld_word)][word_length > 2][, word_length := NULL] ## remove 2 char words
domain_date_prep_dt <- domain_date_prep_dt[!sld_word %in% stop_words_list] ## remove stop words like "and"

domain_date_prep_dt[sld_word == "masques", sld_word := "masque"]
domain_date_prep_dt[sld_word == "artisans", sld_word := "artisan"]
domain_date_prep_dt[sld_word == "covi", sld_word := "covid"]
domain_date_prep_dt[sld_word == "covid", sld_word := "covid19"]


## Remove Outliers
to_remove <- c("plombier", "artisan", # due to many registrations of "artisans-plombier-....fr"
               "goodies", # many domains registered with the same company OVH
               "dec", # protective domain registrations "A-dec"
               "ford") # protective domain registration
domain_date_prep_dt <- domain_date_prep_dt[!sld_word %in% to_remove]

fwrite(domain_date_prep_dt, here("output", paste0(format(Sys.time(), "%Y%M%d_%H%M"), "_afnic_data_trends_prep.csv")))
## Daily aggregates
aggregate_day_dt <- domain_date_prep_dt[, .(word_registration_date = .N), .(sld_word, registration_date, month_date)] ## add searches per day and market

aggregate_day_dt[, total_daily_registrations := .N, .(registration_date)] ## add week counts
aggregate_day_dt[, percent := word_registration_date / total_daily_registrations]

aggregate_day_dt[, year := year(registration_date) + yday(registration_date) / 365]
aggregate_day_dt[, total_word_counts := .N, .(sld_word)]

aggregate_day_dt <- unique(aggregate_day_dt)
aggregate_day_dt <- aggregate_day_dt[total_word_counts > 80] #Threshold, min. #domains registered for a given word

## Calculate linear regression slops of 3 months sliding windows
start_month_list <- seq(as.Date("2019-01-01"), as.Date("2020-05-01"), by = "month")

result_daily_lm_dt_list <- future_lapply(start_month_list, function(start_month) {
  start_month <- as.Date(start_month, "%Y%m%d")
  end_month <- start_month
  month(end_month) <- month(start_month) + 3
  aggregate_day_dt[month_date %between% c(start_month, end_month), as.list(tidy(lm(percent ~ year))), .(sld_word)]
  
})

result_daily_lm_dt <- rbindlist(result_daily_lm_dt_list)

select_growing_dt <- result_daily_lm_dt[term == "year"][order(-estimate)][, head(.SD, 20)][, .(sld_word)]

plot_data_growing_dt <- select_growing_dt[on = aggregate_day_dt, nomatch = 0]

## Visualization ####

# Most relevant words

top_words_list <- unique(plot_data_growing_dt[, .(sld_word, total_word_counts)][order(-total_word_counts)])[, head(.SD, 9)]$sld_word

plot_data_growing_dt[sld_word %in% top_words_list] %>% 
  ggplot(aes(y = percent, x = registration_date, colour = sld_word)) +
  geom_line() +
  scale_x_date(date_labels = "%b '%y") +
  labs(x = "", 
       y = "", 
       title = "<span style='font-size:20pt'>
       <span style='color:#4e4d47'>Fastest growing
       <span style='color:#4e4d47'>words within .fr domains</span>
       </span>",
       subtitle = "<span style='font-size:12pt'>
       <span style='color:#4e4d47'>3 months sliding window
       </span>",
       caption = "Source: Afnic Open Data Aug 2020\nAuthor: @dernapo") +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  facet_wrap(. ~ sld_word, scales = "free_y") +
  theme_words()


ggsave(here("output", paste0(format(Sys.time(),"%Y%m%d"), "_afnic_trends.png")),
       height = 8, width = 12)


