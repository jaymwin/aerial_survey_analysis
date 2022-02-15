
# summarize waterfowl counts

# load libraries ----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(rmarkdown)
source(here::here('scripts/rmarkdown_functions.R')) # rmarkdown plotting, tables, formatting
source(here::here('scripts/waterfowl_survey_functions.R')) # functions for summarizing, analyzing data


# run current year analysis -----------------------------------------------

# what is the current survey year?
# need minus 1 for now to replicate 2021 report
analysis_year <- lubridate::today() %>% lubridate::year() - 1 # -1 just needed for this example


# load data ---------------------------------------------------------------

# connect to database
hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))

# read in metadata with transect/species codes...this will be helpful for prepping data later
transect_metadata <- tbl(hist_db, 'transects_regions') %>%
  collect()
# species_codes <- tbl(hist_db, 'species_codes') %>%
# collect()

# read in raw duck counts
ws_survey_raw <- tbl(hist_db, 'raw_duck_counts') %>%
  collect()

# read in species codes
spp_codes <- tbl(hist_db, 'species_codes') %>%
  collect()

# disconnect
dbDisconnect(hist_db)


# clean data --------------------------------------------------------------

unique(ws_survey_raw$year)

# convert year into 4-digit year
ws_survey_raw <- ws_survey_raw %>%
  mutate(
    year = case_when(
      year >= 73 & year <= 99 ~ year + 1900, # pre-2000 years
      TRUE ~ year + 2000 # >= 2000
    )
  )
ws_survey_raw

# replace empty cells (NAs) in grd column with '4'; this way:
# "4" and "1" = observations from air
# "2" = from ground
# "1" = air count over air-ground portion of transect
ws_survey_raw <- ws_survey_raw %>%
  mutate(grd = replace_na(grd, 4)) # replace NAs with 4 

# replace empty cells (NAs) in wetype column with 10 
# here, "10" represents a "field" sighting rather than a wetland
ws_survey_raw <- ws_survey_raw %>%
  mutate(
    wetype = as.numeric(wetype),
    wetype = case_when(
      is.na(wetype) & sp1 >= 1 ~ 10, # convert NA to 'field'
      TRUE ~ wetype # otherwise keep wetland code
    )
  )

# replace empty species cells (NAs) with 0
ws_survey_raw <- ws_survey_raw %>%
  mutate_at(vars(sp1:bryg), ~replace_na(., 0))

# basin or observation number
ws_survey_raw <- ws_survey_raw %>%
  group_by(transect) %>%
  mutate(basin_no = as.numeric(row_number())) %>%
  ungroup()

# start and end dates (aerial)
aerial_dates <- ws_survey_raw %>%
  select(year, month, day = date, grd) %>%
  # make dates from year/month/day columns
  mutate(date = make_date(year, month, day)) %>%
  filter(year == analysis_year) %>%
  # 1,4 = aerial (1 = air over ground count, 4 = air only)
  # so filter to all aerial surveys
  filter(grd %in% c(1, 4)) %>%
  distinct(date) %>%
  arrange(date)
aerial_dates

# start and end dates (ground)
ground_dates <- ws_survey_raw %>%
  select(year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == analysis_year) %>%
  # ground counts = 2
  filter(grd %in% c(2)) %>% 
  distinct(date) %>%
  arrange(date)
ground_dates

# aerial
# aerial start, end dates
aerial_start_end <- aerial_dates %>%
  # filter to first/last date
  filter(row_number() == 1 | row_number() == n())

# aerial timing summary
aerial_start_survey_date <- aerial_start_end[1, 1] %>% pull()
aerial_end_survey_date <- aerial_start_end[2, 1] %>% pull()
aerial_total_survey_days <- nrow(aerial_dates)

# reformat start, end dates for report
aerial_start_survey_date <- format(aerial_start_survey_date, format = "%B %d") %>%
  str_replace(., ' 0', ' ')
aerial_end_survey_date <- format(aerial_end_survey_date, format = "%B %d") %>%
  str_replace(., ' 0', ' ')

# ground
# ground start, end dates
ground_start_end <- ground_dates %>%
  # filter to first/last date
  filter(row_number() == 1 | row_number() == n())

# ground timing summary
ground_start_survey_date <- ground_start_end[1, 1] %>% pull()
ground_end_survey_date <- ground_start_end[2, 1] %>% pull()
ground_total_survey_days <- nrow(ground_dates)

# reformat start, end dates for report
ground_start_survey_date <- format(ground_start_survey_date, format = "%B %d") %>%
  str_replace(., ' 0', ' ')
ground_end_survey_date <- format(ground_end_survey_date, format = "%B %d") %>%
  str_replace(., ' 0', ' ')

# finally, what's the overlap between air and ground surveys?
ws_transects <- ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == analysis_year) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)
unique(ws_transects$transect)

# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2019) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)


# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2018) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)

# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2017) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)

# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2016) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)

# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2015) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)

# finally, what's the overlap between air and ground surveys?
ws_survey_raw %>%
  select(transect, year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == 2014) %>%
  group_by(transect) %>%
  # filter to ground survey, air over ground surveys
  # 1 = air over ground
  # 2 = ground
  filter(grd %in% c(1, 2)) %>%
  distinct() %>%
  mutate(
    n = n(), # number of surveys per transect
    survey_type = case_when(
      grd == 1 ~ 'aerial', # air over ground
      TRUE ~ 'ground'
    )
  ) %>%
  # just transects with both air and ground
  filter(n == 2) %>%
  arrange(transect, survey_type) %>% print(n=Inf)

### WITH df_r INSTEAD - SEE SCRIPT 2 ###

df_r <- ws_structured

# convert large groups of birds to 0 (assumed to be migrants)
df_r <- df_r %>%
  mutate(
    groups = case_when(
      groups > 19 ~ 0, # 19 used by Ron
      TRUE ~ groups
    )
  )

# subset data to air data over ground, ground data
df_r <- df_r %>%
  filter(grd %in% c(1, 2))

#Calculate the number of indicated birds per transect ; groups excluded for calculating R # "ind_birds_R"
#NOTE: 6-1-21: Calculating "ind_birds_R" needs to be done in a unique dataframe (df_r) that has previously made groups >19 equal to"0".
# Creating a variable like "indicated_birds" in the standard dataframe "WSdata_str" would not be useful because there are subsequent calculations that rely on varying calculations of indicated birds that are not the same as below.

df_r <- df_r %>%
  mutate(
    ind_birds_R = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake + groups,
      species == 81 ~ (pairs * 2) + flockdrake + lonedrake, # Ron doesn't count groups of swans
      TRUE ~ ((pairs + flockdrake + lonedrake) * 2) + groups
    )
  )


# filter to air/ground counts
df_r <- df_r %>% 
  filter(grd >= 1)
df_r

df_transects <- df_r %>%
  select(year, transect, grd) %>%
  distinct() %>%
  filter(year == 2021) %>%
  group_by(transect) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>%
  arrange(transect, grd) %>%
  print(n=Inf)
length(unique(df_transects$transect))

