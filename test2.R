
library(tidyverse)
library(lubridate)

df <- read_csv(here::here('raw_data/annual_survey_data/2021_waterfowl.csv'))

unique(df$sp1)

df %>%
  filter(is.na(sp1) & is.na(grd))

df <- df %>%
  select(year, month, date, transect, grd) %>%
  mutate(
    year = case_when(
      year >= 73 & year <= 99 ~ year + 1900, # pre-2000 years
      TRUE ~ year + 2000 # >= 2000
    )
  ) %>%
  mutate(date = make_date(year, month, date)) %>%
  select(-year, -month)
df

unique(df$grd)

df <- df %>%
  mutate(grd = replace_na(grd, 4)) %>%
  distinct()
df

df %>%
  filter(grd %in% c(1, 2)) %>%
  mutate(year = year(date)) %>%
  group_by(transect) %>%
  mutate(n = n()) %>%
  filter(n == 2)
