

# load libraries ----------------------------------------------------------

# options(warn = -1) # turn off warnings for now
library(lubridate)
library(tidyverse)
library(rmarkdown)
source(here::here('scripts/rmarkdown_functions.R')) # rmarkdown plotting, tables, formatting
source(here::here('scripts/waterfowl_survey_functions.R')) # functions for summarizing, analyzing data


# run current year analysis -----------------------------------------------

# what is the current survey year?
analysis_year <- 
  today() %>%
  year()

# append new data to database, summarize count data, and run state-space analyses
analyze_survey_data(analysis_year)


# now write year-appropriate report ---------------------------------------

# create new, year-specific report template
create_new_report_rmd(analysis_year)

# at this point, go in and manually change text in current year report...

# then generate final report
render_report(
  year = analysis_year, # appropriate year to dynamically update title, text, etc.
  report_type = 'oas'
)
