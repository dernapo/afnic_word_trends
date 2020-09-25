###################################################################
## We will download and read the full list of .fr
###################################################################

pacman::p_load(data.table, here, dplyr)

year_month <- "202008"

url <- paste0("https://www.afnic.fr/data/opendata/", year_month, "_OPENDATA_A-NomsDeDomaineEnPointFr.zip")
#url <- 'https://www.afnic.fr/data/opendata/202008_OPENDATA_A-NomsDeDomaineEnPointFr.zip'


download.file(url, destfile = here("data", "afnic.zip"))

unzip(here("data", "afnic.zip"), exdir = here("data"))


afnic_file <- max(list.files(here("data"), pattern = "NomsDeDomaineEnPointFr.csv$", full.names = TRUE))


afnic_dt <- fread(afnic_file)


## Adjust col names
afnic_dt <- janitor::clean_names(afnic_dt)

setnames(afnic_dt, 
         old = c("nom_de_domaine",
                 "date_de_cr_ation",
                 "date_de_retrait_du_whois"),
         new = c("domain_name", 
                 "registration_date",
                 "cancellation_date"),
         skip_absent = TRUE)

## Adjust dates
afnic_dt[, registration_date := as.Date(registration_date, "%d-%m-%Y")]
afnic_dt[, cancellation_date := as.Date(cancellation_date, "%d-%m-%Y")]



#afnic_selection <- afnic_dt[date_de_retrait_du_whois == "" & pays_titulaire == "FR", .(count = .N), .(departement_titulaire, type_du_titulaire)]

#fwrite(afnic_selection, here("output", paste0(year_month, "_fr_domains.csv")))