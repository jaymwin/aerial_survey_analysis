

# load libraries ----------------------------------------------------------

library(lubridate)
library(fs)
library(tidyverse)
library(DBI)
library(dbplyr)
library(RSQLite)

source(here::here('scripts/waterfowl_survey_functions.R')) # functions for summarizing, analyzing data

# analysis_year <- 2021


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

