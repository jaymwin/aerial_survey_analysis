

# load libraries ----------------------------------------------------------

# options(warn = -1) # turn off warnings for now
library(lubridate)
library(tidyverse)
library(rmarkdown)
source(here::here('scripts/rmarkdown_functions.R')) # rmarkdown plotting, tables, formatting
source(here::here('scripts/waterfowl_survey_functions.R')) # functions for summarizing, analyzing data


# run current year analysis -----------------------------------------------

# what is the current survey year?
# need minus 1 for now to replicate 2021 report
analysis_year <- lubridate::today() %>% 
  lubridate::year() # - 1 # -1 just needed for this example 2021

# append new data to database, summarize count data, run state-space analyses
analyze_survey_data(analysis_year)


# now write year-appropriate report -----------------------------------

# generate final report (OAS)
render_report(
  year = analysis_year, # appropriate year to dynamically update title, text
  report_type = 'oas'
)

# # final report with fancier formatting
# render(
#   here::here("scripts/stylized_rmarkdown/formatted_final_report.Rmd"), # .rmd file for fancier looking report
#   params = list(
#     year = analysis_year # appropriate year to dynamically update title, text
#   ),
#   output_dir = here::here(str_c('reports', '/', analysis_year)), # year-specific output directory
#   output_file = str_c("formatted_report_", analysis_year, ".pdf"), # year-specific report name
#   quiet = TRUE
# )

