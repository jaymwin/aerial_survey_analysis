
# summarize waterfowl counts

# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

# analysis_year <- 2021 # lubridate::year(Sys.Date())


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

# convert year into 4-digit year
ws_survey_raw <- ws_survey_raw %>%
  mutate(
    year = case_when(
      # year > 21 ~ year + 1900, # pre-2000 years ### THIS COULD BE A PROBLEM IN >2022 SO REMOVED ###
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
ws_survey_raw

# replace empty cells (NAs) in wetype column with 10 
# here, "10" represents a "field" sighting rather than a wetland
ws_survey_raw <- ws_survey_raw %>%
  mutate(
    wetype = as.numeric(wetype),
    wetype = case_when(
      is.na(wetype) & sp1 >= 1 ~ 10, # convert NA to field
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
ws_survey_raw

# save for summarizing wetlands in next script
ws_survey_raw %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'ws_survey_raw', '.csv'))

# pivot data to a longer format - observation for each species code
ws_structured <- ws_survey_raw %>%
  select(year, date, transect, grd, wetype, basin_no, starts_with(c('sp', 'pr', 'lm', 'fm', 'us'))) %>%
  pivot_longer(
    c(-year, -date, -transect, -grd, -wetype, -basin_no), # keep these in place
    names_to = c(".value", "set"), # value = species code or count; set = 1-9 for priority species code
    names_pattern = "([A-Za-z]+)(\\d+)" # sp, pr, ...
  ) %>%
  select(-set) %>% # not needed
  rename(species = sp, pairs = pr, lonedrake = lm, flockdrake = fm, groups = us)
ws_structured

# join with transect metadata to add region
ws_structured <- ws_structured %>%
  left_join(., transect_metadata)
ws_structured

# create priority species codes from species codes
ws_structured <- ws_structured %>%
  mutate(
    p_species = case_when(
      species == 32 ~ 1, # mallard
      species == 40 ~ 2, # blue-winged teal
      species == 44 ~ 3, # wood duck
      species == 72 ~ 4, # canada goose
      species < 29 | species %in% c(45, 49, 53, 63, 66) | species >= 72 ~ 9, # non-waterfowl
      TRUE ~ 5 # all other duck species
    )
  )
ws_structured

# remove observations where no species was recorded (i.e unoccupied wetland observations)
ws_structured <- ws_structured %>%
  filter(species != 0)
ws_structured

# Adjusting for reports of "1" flocked drake - according to R. Gatti analysis
ws_structured <- ws_structured %>%
  mutate(
    lonedrake = case_when(
      flockdrake == 1 ~ lonedrake + flockdrake,
      TRUE ~ lonedrake
    ),
    flockdrake = case_when(
      flockdrake == 1 ~ 0,
      TRUE ~ flockdrake
    )
  )
ws_structured

#Treats groups less than 5 as identifiable pairs, with the exception of CAGO.  
ws_structured <- ws_structured %>%
  mutate(
    pairs = case_when(
      p_species !=4 & groups > 0 & groups < 5 ~ (pairs) + as.integer((groups / 2)),
      TRUE ~ pairs
    ),
    lonedrake = case_when(
      p_species !=4 & groups > 0 & groups < 5 ~ lonedrake + (groups - 2 * (as.integer((groups / 2)))),
      TRUE ~ lonedrake
    ),
    groups = case_when(
      p_species !=4 & groups > 0 & groups < 5 ~ 0,
      TRUE ~ groups
    )
  )
ws_structured

#check the structure of the ws_structured
glimpse(ws_structured)

# save for VCF calculation 
ws_structured %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'ws_structured', '.csv'))


###########Create Copies of WSdata_Str for varying objectives in subsequent analyses###################################

###Copy 1 - AIR DATA ONLY ### 
ws_structured_air <- ws_structured %>%
  filter(grd %in% c(1, 4))

#Calculate the number of "indicated birds" per transect for calculating total population # "ind_birds_Pop"
ws_structured_air <- ws_structured_air %>%
  mutate(
    ind_birds_Pop = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake + groups, # Redheads, Cans , Ring-necks, and Ruddy ducks
      TRUE ~ ((pairs + flockdrake + lonedrake) * 2) + groups
    )
  )

#Calculate the number of "seen birds" per transect for calculating total population # "seen_birds_Pop" - Basically all counted the same (<- (pairs*2) + lonedrake + Flockdrake+Groups); just kept the format for different species though
ws_structured_air <- ws_structured_air %>%
  mutate(
    seen_birds_Pop = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake + groups, # Redheads, Cans , Ring-necks, and Ruddy ducks
      TRUE ~ (pairs * 2) + flockdrake + lonedrake + groups
    )
  )

#Calculate the number of "indicated birds without the groups" per transect for calculating total population # "oldind_birds_Pop"
ws_structured_air <- ws_structured_air %>%
  mutate(
    oldind_birds_Pop = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake, # Redheads, Cans , Ring-necks, and Ruddy ducks
      TRUE ~ (pairs + flockdrake + lonedrake) * 2
    )
  )

###Copy 2 - AIR and GROUND Data (GRD = 1,2,and4) ### 

#Calculate "numbirds" according to Ron's "sppcheck.sas" file. Basically does not discriminate calculation among species, same as "seen_birds_Pop" above
#Calculate "numbirds" according to Ron's "sppcheck.sas" file. Basically does not discriminate calculation among species, same as "seen_birds_Pop" above
ws_structured_air_ground <- ws_structured %>%
  mutate(
    numbirds_Pop = (pairs * 2) + flockdrake + lonedrake + groups
  )

#Calculate the number of "indicated birds" per transect for calculating total population # "ind_birds_Pop"
ws_structured_air_ground <- ws_structured_air_ground %>%
  mutate(
    ind_birds_Pop = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs*2) + flockdrake + lonedrake + groups, # Redheads, Cans , Ring-necks, and Ruddy ducks
      TRUE ~ ((pairs + flockdrake + lonedrake) * 2) + groups
    )
  )


######Filter Data Sets for Relevant Queries ################

####### Subset Data to those recorded in Groups seen by AIR
ws_structured_air_groups <- ws_structured_air %>%
  filter(groups >= 5)

#Summarize into table by Region
wsds_groups <- ws_structured_air_groups %>%
  group_by(year, region, p_species) %>%
  summarize(group = sum(groups)) %>%
  mutate(
    region = case_when(
      region == 1 ~ 'SEC',
      region == 2 ~ 'NHI',
      region == 3 ~ 'NLO',
      region == 4 ~ 'SWD'
    )
  ) %>%
  pivot_wider(names_from = region, values_from = group) %>%
  ungroup() %>%
  arrange(year, p_species) %>%
  select(year, p_species, SEC, NHI, NLO, SWD)


#Check Groups in Year of Interest by row
group_21 <- ws_structured_air_groups %>%
  filter(year == analysis_year)
#Export to Excel to include as anecdotal support in summary report

# write.csv(Group21,"./Annual Report Data/2021 WSDS Results/Group Identification.csv")

# Group21 %>%
#   write_csv(here::here('output/annual_report_data/2021_WSDS_results/group_identification.csv'))

#Subset Down to the Year of Interest - CHANGE ANNUALLY
wsds_groups_21 <- wsds_groups %>%
  filter(year == analysis_year)

wsds_groups_21 %>%
  replace(is.na(.), 0) %>%
  mutate(total = SEC + NHI + NLO + SWD)

####### Summarize into table for all indicated birds seen by AIR (Includes birds counted in all groups)
wsds_birds <- ws_structured_air %>%
  group_by(year, region, p_species) %>%
  summarize(ind_birds_Pop = sum(ind_birds_Pop)) %>%
  mutate(
    region = case_when(
      region == 1 ~ 'SEC',
      region == 2 ~ 'NHI',
      region == 3 ~ 'NLO',
      region == 4 ~ 'SWD'
    )
  ) %>%
  pivot_wider(names_from = region, values_from = ind_birds_Pop) %>%
  ungroup() %>%
  arrange(year, p_species) %>%
  select(year, p_species, SEC, NHI, NLO, SWD)

wsds_birds_21 <- wsds_birds %>%
  filter(year == analysis_year)

########### For comparison to 2021

previous_year <- wsds_groups %>%
  distinct(year) %>%
  slice_tail(n = 2) %>%
  slice(1) %>%
  pull()

wsds_groups_19 <- wsds_groups %>%
  filter(year == previous_year)

wsds_birds_19 <- wsds_birds %>%
  filter(year == previous_year)

#Group Tables for Export

wsds19_sum <- wsds_groups_19 %>%
  bind_rows(wsds_birds_19)

wsds21_sum <- wsds_groups_21 %>%
  bind_rows(wsds_birds_21)

#Export to Excel for final formatting and percent calculation
# write.csv(WSDS19_Sum, "WSDS19_Sum.csv")

# write.csv(WSDS21_Sum,"./Annual Report Data/2021 WSDS Results/WSDS21_Sum.csv")

# WSDS21_Sum %>%
#   write_csv(here::here('output/annual_report_data/2021_WSDS_results/WSDS21_Sum.csv'))


#Summarizing total Seen P-Species, by Air only, by Year; then Ground Only - CHANGE YEARS AS NECESSARY
air_seen <- ws_structured_air %>%
  filter(year %in% c(previous_year, analysis_year)) %>%
  group_by(year, p_species) %>%
  summarise(seen_birds_Pop = sum(seen_birds_Pop))

# write.csv(Air_Seen, "Air_Seen.csv")

ground_seen <- ws_structured_air_ground %>%
  filter(grd == 2) %>%
  filter(year %in% c(previous_year, analysis_year)) %>%
  group_by(year, p_species) %>%
  summarise(numbirds_Pop = sum(numbirds_Pop))

# write.csv(Ground_Seen, "Ground_Seen.csv")

#NOTE: 6-1-21 Shouldn't the following code that counts composition of species, coots, cranes, and swans come from "ws_structured_air" and not "ws_structured_air_ground"??
#Since "ws_structured_air" is AIR data only, it seems like the better data set.  "ws_structured_air_ground" allows for individuals both observed in the air and ground to be double counted. 
#IF I switch to "ws_structured_air" then I'll need to update the numbers to Taylor for previous years.  

#NOTE: 6-14-21 Going forward and switching code to use "ws_structured_air" instead of "ws_structured_air_ground".  This switch will only count the composition of species indicated from the AIR. 

# % Composition of species making up indicated O_Ducks - CHANGE YEAR AS NECESSARY
comp_oducks_region <- ws_structured_air %>%
  filter(p_species == 5 & year == analysis_year & groups < 20) %>%
  group_by(region, species) %>%
  summarize(ind_birds_Pop = sum(ind_birds_Pop))
# write.csv(Comp_ODucks_Region, "Comp_ODucks_Region.csv")

comp_oducks_state <- ws_structured_air %>%
  filter(p_species == 5 & year == analysis_year & groups < 20) %>%
  group_by(species) %>%
  summarize(ind_birds_Pop = sum(ind_birds_Pop))

comp_oducks_state %>%
  mutate(percent = round((ind_birds_Pop / sum(ind_birds_Pop)) * 100), 0) %>%
  left_join(., spp_codes) %>%
  select(common_name, percent) %>%
  mutate(
    common_name = make_clean_names(common_name),
    percent = str_c(percent, '%')
    ) %>%
  pivot_wider(names_from = common_name, values_from = percent) %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'comp_oducks_state', "_", analysis_year, '.csv'))


# summarize coots, cranes, and swans --------------------------------------

# count coots by year and region (air only)
coots <- ws_survey_raw %>%
  filter(coots != 0 & grd != 2) %>%
  group_by(year) %>%
  summarize(coots = sum(coots)) %>%
  slice_tail(n = 5)

# save for reporting
coots %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'coots', "_", analysis_year, '.csv'))

# count sandhill cranes by year and region (air only)
shcranes_total <- ws_structured_air %>%
  filter(species == 86) %>%
  group_by(year) %>%
  summarize(seen_birds_Pop = sum(seen_birds_Pop))

shcranes_no_groups <- ws_structured_air %>%
  filter(species == 86 & groups == 0) %>%
  group_by(year) %>%
  summarize(seen_birds_Pop = sum(seen_birds_Pop))

shcranes <- shcranes_total %>%
  rename(cranes_total = seen_birds_Pop) %>%
  left_join(., shcranes_no_groups %>% rename(cranes_no_groups = seen_birds_Pop))

# save for reporting
shcranes %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'cranes', "_", analysis_year, '.csv'))

# count whooping cranes by year and region (air only)
whcranes <- ws_structured_air %>%
  filter(species == 84 & year >= 2016)

# count trumpeter swans by year and region (air only)
trumpeter_swans <- ws_structured_air %>%
  filter(species == 81 & groups < 5 & year >= 2016) %>%
  group_by(year) %>%
  summarize(seen_birds_Pop = sum(seen_birds_Pop))

# save for reporting
trumpeter_swans %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'trumpeter_swans', "_", analysis_year, '.csv'))


# survey timing -----------------------------------------------------------

# grd codes:
# 1 = air over ground observations
# 2 = ground observations
# 4 = air only observations

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

# start and end dates (ground)
ground_dates <- ws_survey_raw %>%
  select(year, month, day = date, grd) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(year == analysis_year) %>%
  # ground counts = 2
  filter(grd %in% c(2)) %>% 
  distinct(date) %>%
  arrange(date)

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
air_ground_overlap <- ws_survey_raw %>%
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
  arrange(transect, survey_type) %>%
  mutate(diff = as.numeric(date - lag(date))) %>%
  drop_na()

# table of survey timing to report later
survey_timing <- tibble(
  aerial_start_survey_date = aerial_start_survey_date,
  aerial_end_survey_date = aerial_end_survey_date,
  aerial_total_survey_days = aerial_total_survey_days,
  ground_start_survey_date = ground_start_survey_date,
  ground_end_survey_date = ground_end_survey_date,
  ground_total_survey_days = ground_total_survey_days,
  air_ground_overlap = max(abs(air_ground_overlap$diff)) # sometimes done before aerial survey so abs
)
survey_timing

# save for reporting
survey_timing %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'survey_timing', "_", analysis_year, '.csv'))

