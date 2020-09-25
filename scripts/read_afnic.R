###################################################################
## We will download and read the full list of .fr
###################################################################

## Load libraries ####
pacman::p_load(data.table, here, dplyr)

## Load configuration variables ####
year_month <- "202008"
url <- paste0("https://www.afnic.fr/data/opendata/", year_month, "_OPENDATA_A-NomsDeDomaineEnPointFr.zip")
#url <- 'https://www.afnic.fr/data/opendata/202008_OPENDATA_A-NomsDeDomaineEnPointFr.zip'

## Download data ####
download.file(url, destfile = here("data", "afnic.zip"))
unzip(here("data", "afnic.zip"), exdir = here("data"))

## Load data ####
afnic_file <- max(list.files(here("data"), pattern = "NomsDeDomaineEnPointFr.csv$", full.names = TRUE))

afnic_dt <- fread(afnic_file)

## Clean up files and variables ####
unlink(here("data", "afnic.zip"))
unlink(afnic_file)
rm(afnic_file, url, year_month)

## Clean up data ####
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

