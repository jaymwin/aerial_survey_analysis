

# load libraries ----------------------------------------------------------

library(lubridate)
library(fs)
library(tidyverse)
library(DBI)
library(dbplyr)
library(RSQLite)
library(haven)
library(janitor)

source(here::here('scripts/waterfowl_survey_functions.R')) # functions for summarizing, analyzing data

# analysis_year <- 2021


# import data from sas (2022-onward) --------------------------------------

if (analysis_year >= 2022) {

sas_files <- dir_ls(here::here('raw_data/annual_survey_data'), glob = '*sas7bdat') %>%
  as_tibble() %>%
  mutate(
    type = str_extract(value, 'airwet|wsds'),
    name = str_remove(basename(value), '.sas7bdat'),
    year = str_sub(name, start = -2, end = -1),
    year = str_c(20, year)
  )
sas_files

# waterfowl
sas_files %>%
  filter(type == 'wsds' & year == analysis_year) %>%
  pull(value) %>%
  read_sas() %>%
  filter(YEAR == str_sub(analysis_year, start = 3, end = 4)) %>%
  clean_names() %>%
  mutate(across(-c(direct, side), as.numeric)) %>%
  write_csv(str_c(here::here('raw_data/annual_survey_data'), '/', analysis_year, '_waterfowl.csv'))

# wetlands
sas_files %>%
  filter(type == 'airwet' & year == analysis_year) %>%
  pull(value) %>%
  read_sas() %>%
  filter(YEAR == analysis_year) %>%
  clean_names() %>%
  # get rid of any NA rows
  filter(!is.na(month)) %>%
  write_csv(str_c(here::here('raw_data/annual_survey_data'), '/', analysis_year, '_air_wetlands.csv'))

}


# append new spring survey data (waterfowl) -------------------------------

# connect to historical database
hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))

# view tables
src_dbi(hist_db)

# append new raw waterfowl count data
# this might need to be tweaked from Taylor
new_waterfowl_counts <- read_csv(str_c(here::here('raw_data/annual_survey_data'), '/', analysis_year, '_waterfowl.csv'))

# append; if year already in DB, then function will stop this
# DBI::dbWriteTable(hist_db, "raw_duck_counts", new_waterfowl_counts, append = TRUE) # essentially what is happening with update_database()
update_database(db = hist_db, table = "raw_duck_counts", data_to_append = new_waterfowl_counts)


# append new spring survey data (wetlands) -------------------------------------------

# append new raw wetland count data
new_wetland_counts <- read_csv(str_c(here::here('raw_data/annual_survey_data'), '/', analysis_year, '_air_wetlands.csv'))

# append; if year already in DB, then function will stop this
# DBI::dbWriteTable(hist_db, "raw_wetland_counts", new_wetland_counts, append = TRUE)
update_database(db = hist_db, table = "raw_wetland_counts", data_to_append = new_wetland_counts)

# disconnect
dbDisconnect(hist_db)

