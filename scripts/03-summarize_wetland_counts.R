

# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)


# read in wetland count data ----------------------------------------------

# load database
hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))

# read in duck counts
ws_survey_raw <- read_csv(
  str_c(here::here('output'), '/', analysis_year, '/', 'ws_survey_raw', '.csv'),
  col_types = cols(
    .default = "d", 
    direct = 'c', 
    side = 'c')
)
ws_survey_raw

# occupied wetlands only
occ_wet <- ws_survey_raw %>%
  filter(sp1 != 0 & grd != 2) %>% # species not = 0 and transect not = ground
  mutate(
    region = case_when(
      transect < 30 ~ 1,
      transect >= 30 & transect < 43 ~ 2,
      transect >= 43 & transect < 61 ~ 3,
      transect >= 61 & transect < 72 ~ 4
    )
  )
occ_wet

# read in aerial wetland counts from database
air_wet <- tbl(hist_db, 'raw_wetland_counts') %>%
  collect() %>%
  mutate(across(everything(), ~replace_na(.x, 0))) # replace na counts with 0s
air_wet

# get rid of ground wetland counts here
air_wet <-
  air_wet %>%
  filter(ground != 1)

# disconnect
dbDisconnect(hist_db)


# generate wetland summaries ----------------------------------------------

# use summarize_wetlands() function to extract wetland summaries
wetland_estimates <- summarize_wetlands(air_wet, occ_wet)
all_occupied_wetlands <- wetland_estimates[[1]]
all_unoccupied_wetlands <- wetland_estimates[[2]]
all_total_wetlands <- wetland_estimates[[3]]
all_wetlands_psqm <- wetland_estimates[[4]]
all_sum_wetlands_psqm <- wetland_estimates[[5]]

# # save wetland summary for analysis year
# wetland_estimates %>%
#   write_rds(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.rds'))

# data frame for wetland summary tables/figures in internal report
all_wetlands_psqm <- all_wetlands_psqm %>%
  group_by(year, region) %>%
  summarize(
    i_ii_vi = sum(c(typ1, typ2, typ6)),
    iii = sum(c(typ3)),
    iv_v = sum(c(typ4, typ5)),
    vii_viii = sum(c(typ7, typ8)),
    non_linear = sum(c(typ1, typ2, typ3, typ4, typ5, typ6, typ7, typ8)),
    stream = sum(stream),
    ditch = sum(ditch),
    linear = sum(c(stream, ditch))
  ) %>%
  ungroup()

# save
all_wetlands_psqm %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.csv'))
