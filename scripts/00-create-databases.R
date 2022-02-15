

# load libraries ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(dbplyr) # for database
library(DBI) # for database
library(RSQLite) # for database
library(janitor)
library(sf)
library(fs)
library(rnaturalearth)
source(here::here('scripts/waterfowl_survey_functions.R'))


# first, setup directories for storing output, figures, and reports ------------------------------------

# determine current year of analysis
folder_names <- c('output', 'reports')

# create folders for each year to store report, other products later
folder_names %>%
  walk(dir_create(here::here(str_c(., '/', analysis_year))))


# create database of 'historical' (1973-2019) duck counts, wetlands -------

# only do this if the databases don't exist already
database_dir <- here::here('databases')

if (!dir.exists(database_dir)) {
  
  print('create new databases with 1973-2019 data...')
  create_new_databases()
  
} else {
  
  print('databases already exist, proceed to appending new data...')
  
}
  
