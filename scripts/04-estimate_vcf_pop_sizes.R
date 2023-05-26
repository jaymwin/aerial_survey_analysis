
library(tidyverse)
library(lubridate)
source(here::here('scripts/waterfowl_survey_functions.R'))

# analysis_year <- 2021 # lubridate::year(Sys.Date())
hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))

# prep data to calculate visibility correction factor (VCF) ---------------

# load structured data created in script 2
ws_str <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'ws_structured', '.csv'))
df_r <- ws_str

# convert large groups of birds to 0 (assumed to be migrants)
df_r <- 
  df_r %>%
  mutate(
    groups = case_when(
      groups > 19 ~ 0, # 19 used by Ron
      TRUE ~ groups
    )
  )

# subset data to air data over ground, ground data
df_r <- 
  df_r %>%
  filter(grd %in% c(1, 2))

#Calculate the number of indicated birds per transect ; groups excluded for calculating R # "ind_birds_R"
#NOTE: 6-1-21: Calculating "ind_birds_R" needs to be done in a unique dataframe (df_r) that has previously made groups >19 equal to"0".
# Creating a variable like "indicated_birds" in the standard dataframe "WSdata_str" would not be useful because there are subsequent calculations that rely on varying calculations of indicated birds that are not the same as below.

df_r <- 
  df_r %>%
  mutate(
    ind_birds_R = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake + groups,
      species == 81 ~ (pairs * 2) + flockdrake + lonedrake, # Ron doesn't count groups of swans
      TRUE ~ ((pairs + flockdrake + lonedrake) * 2) + groups
    )
  )


# VCF calculations --------------------------------------------------------

# filter to air/ground counts
df_r <- 
  df_r %>% 
  filter(grd >= 1)

# save for air vs ground plotting
df_r %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_air_vs_ground', '.csv'))

# calculate, combine vcfs for all species
all_ducks_vcf <- 
  tibble(
  spp = seq(1, 5, 1) # priority species codes
  ) %>%
  mutate(vcf = map(spp, ~calculate_vcf(df = df_r, spp = .x))) %>% # calculate VCF for each species
  unnest(vcf)

# save vcf information
all_ducks_vcf %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_vcf_estimates', '.csv'))
  

# population estimates ----------------------------------------------------

# subset all species records for observations that are from the air survey over all years for calculating Population size
df_pop <- 
  ws_str %>%
  filter(grd %in% c(1, 4)) %>%
  mutate(
    groups = case_when(
      groups > 19 ~ 0,
      TRUE ~ groups
    )
  )

df_pop <- 
  df_pop %>%
  mutate(
    ind_birds_Pop = case_when(
      species %in% c(46, 47, 50, 67) ~ (pairs * 2) + flockdrake + lonedrake + groups,
      species == 81 ~ (pairs * 2) + flockdrake + lonedrake, # Ron doesn't count groups of swans
      TRUE ~ ((pairs + flockdrake + lonedrake) * 2) + groups
    )
  )

# # Summarizing total Indicated P-Species, by Air only, by Year - Without Groups >19
# Air_Indicated <- df_pop %>% 
#   group_by(year, p_species) %>% 
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))
# 
# # priority spp 1
# Air_IndicatedP1 <- df_pop %>%
#   filter(p_species == 1) %>% 
#   group_by(year, region) %>% 
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))
# 
# # priority spp 2
# Air_IndicatedP2 <- df_pop %>%
#   filter(p_species == 2) %>%
#   group_by(year, region) %>%
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))
# 
# # priority spp 3
# Air_IndicatedP3 <- df_pop %>%
#   filter(p_species == 3) %>%
#   group_by(year, region) %>%
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))
# 
# # priority spp 4
# Air_IndicatedP4 <- df_pop %>%
#   filter(p_species == 4) %>%
#   group_by(year, region) %>%
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))
# 
# # priority spp 5
# Air_IndicatedP5 <- df_pop %>%
#   filter(p_species ==5) %>% 
#   group_by(year, region) %>% 
#   summarize(ind_birds_Pop = sum(ind_birds_Pop))

df_pop_current_year <- 
  df_pop %>%
  filter(year == analysis_year)

#Summary Swan Stats: ### ARE THESE USED AGAIN ###?
swans1 <- 
  df_pop_current_year %>% filter(species == 81) %>% 
  group_by(region) %>% 
  summarize(ind_birds_Pop = sum(ind_birds_Pop)) %>%
  ungroup()

swans2 <- 
  df_pop_current_year %>% filter(species == 81) %>% 
  group_by(region, transect) %>% 
  summarize(ind_birds_Pop = sum(ind_birds_Pop)) %>%
  ungroup()
  

# ducks and geese - by region
  
# create a data frame of duck/geese species by region to iterate over
df_pop <- 
  tibble(
  species_code = rep(seq(1, 5, 1), times = 4),
  region_num = rep(seq(1, 4, 1), each = 5)
)

# estimate duck/geese species populations by region
df_summary <- 
  df_pop %>%
  mutate(
    pop = map2(
      species_code, 
      region_num, 
      ~estimate_population_size_by_region(
        count_df = df_pop_current_year, 
        vcf_df = all_ducks_vcf, 
        species_code = .x, 
        region_num = .y
      )
    )
  ) %>% # calculate VCF for each species
  unnest(pop)

# clean up
df_summary <- 
  df_summary %>%
  select(species:total_air, sqm:cv_b, b, area, r, pop_est, pop_var, pop_se_1, pop_se_2) %>%
  mutate(pop_est = round(pop_est, 0))

# save this; needed for table 5 of internal report
df_summary %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_summary', '.csv'))

# summarize for whole state, analysis year
df_summary <- 
  df_summary %>%
  select(species, pop_est) %>%
  group_by(species) %>%
  summarize(n = sum(pop_est)) %>%
  mutate(
    species = case_when(
      species == 1 ~ 'mallard',
      species == 2 ~ 'blue-winged teal',
      species == 3 ~ 'wood duck',
      species == 4 ~ 'canada goose',
      TRUE ~ 'other ducks'
    ),
    year = analysis_year
  ) %>%
  select(year, species, n)

# tally total ducks
total_ducks <- 
  tibble(
  year = analysis_year,
  species = 'total ducks',
  n = sum(df_summary %>% filter(species != 'canada goose') %>% pull(n)) # add everything except geese together
)

df_summary <- 
  df_summary %>%
  bind_rows(., total_ducks)

# append to database
# DBI::dbWriteTable(hist_db, "vcf_corrected_pop_estimates", df_summary, append = TRUE)
update_database(db = hist_db, table = "vcf_corrected_pop_estimates", data_to_append = df_summary)


# swans - by region

# create a data frame of swans by region to iterate over
df_swan_pop <- 
  tibble(
  species_code = rep(9, times = 4), # tundra swan code
  region_num = seq(1, 4, 1)
)

# estimate swan populations by region
df_swan_summary <- 
  df_swan_pop %>%
  mutate(
    pop = map(region_num, ~estimate_population_size_swans(count_df = df_pop_current_year %>% filter(species == 81), region_num = .x)
    )
  ) %>% # calculate VCF for each species
  unnest(pop)

# clean up
df_swan_summary <- 
  df_swan_summary %>%
  select(species:total_air, sqm:cv_b, b, area, pop_est, pop_var, pop_se_1, pop_se_2) %>%
  mutate(pop_est = round(pop_est, 0))

# save this
df_swan_summary %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_swan_summary', '.csv'))

# summarize for whole state, analysis year
df_swan_summary <- 
  df_swan_summary %>%
  select(species, pop_est) %>%
  group_by(species) %>%
  summarize(n = sum(pop_est)) %>%
  mutate(
    species = case_when(
      species == 81 ~ 'trumpeter swan'
    ),
    year = analysis_year
  ) %>%
  select(year, species, n)

# append to database
# DBI::dbWriteTable(hist_db, "c", df_swan_summary, append = TRUE)
update_database(db = hist_db, table = "swan_pop_estimates", data_to_append = df_swan_summary)

tbl(hist_db, 'swan_pop_estimates') %>%
  collect() %>%
  arrange(species, year) %>%
  tail()


# ducks - statewide

# create a data frame of duck/geese species by region to iterate over
df_pop_statewide <- 
  tibble(
  species_code = seq(1, 5, 1)
)

# estimate duck/geese species populations by region
df_summary_statewide <- 
  df_pop_statewide %>%
  mutate(
    pop = map(species_code, ~estimate_population_size_statewide(count_df = df_pop_current_year, vcf_df = all_ducks_vcf, species_code = .x)
    )
  ) %>% # calculate VCF for each species
  unnest(pop)

# clean up
df_summary_statewide <- 
  df_summary_statewide %>%
  select(species:total_air, sqm:cv_b, b, area, r, pop_est, pop_var, pop_se_1, pop_se_2) %>%
  mutate(pop_est = round(pop_est, 0))

# and disconnect when finished
dbDisconnect(hist_db)
