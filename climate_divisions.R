
library(tidyverse)
library(readxl)
library(sf)

counties <- read_sf('/Users/Jay/Desktop/County_Boundaries_24K/County_Boundaries_24K.shp')
counties

ggplot() +
  geom_sf(data = counties) +
  theme_minimal()

unique(counties$COUNTY_NAM)

counties <- counties %>%
  mutate(
    division = case_when(
      COUNTY_NAM %in% c('Douglas', 'Bayfield', 'Burnett', 'Washburn', 'Sawyer', 'Polk', 'Barron', 'Rusk', 'Chippewa') ~ 1,
      COUNTY_NAM %in% c('Ashland', 'Iron', 'Vilas', 'Oneida', 'Price', 'Lincoln', 'Taylor', 'Clark', 'Marathon') ~ 2,
      COUNTY_NAM %in% c('Florence', 'Forest', 'Marinette', 'Langlade', 'Menominee', 'Oconto', 'Shawano') ~ 3,
      COUNTY_NAM %in% c('Saint Croix', 'Dunn', 'Pierce', 'Pepin', 'Eau Claire', 'Jackson', 'Monroe', 'La Crosse', 'Trempealeau', 'Buffalo') ~ 4,
      COUNTY_NAM %in% c('Wood', 'Portage', 'Waupaca', 'Juneau', 'Adams', 'Waushara', 'Marquette', 'Green Lake') ~ 5,
      COUNTY_NAM %in% c('Door', 'Kewaunee', 'Brown', 'Outagamie', 'Winnebago', 'Calumet', 'Manitowoc', 'Sheboygan', 'Fon Du Lac') ~ 6,
      COUNTY_NAM %in% c('Vernon', 'Richland', 'Crawford', 'Sauk', 'Iowa', 'Grant', 'Lafayette') ~ 7,
      COUNTY_NAM %in% c('Columbia', 'Dodge', 'Dane', 'Jefferson', 'Rock', 'Green') ~ 8,
      TRUE ~ 9
    )
  )
counties

ggplot() +
  geom_sf(data = counties, aes(fill = as.factor(division)), alpha = 0.7) +
  theme_minimal()

counties <- counties %>%
  group_by(division) %>%
  summarise()
counties

ggplot() +
  geom_sf(data = counties, aes(fill = as.factor(division)), alpha = 0.7) +
  theme_minimal()

counties <- counties %>%
  mutate(
    division_name = case_when(
      division == 1 ~ 'NW',
      division == 2 ~ 'NC',
      division == 3 ~ 'NE',
      division == 4 ~ 'WC',
      division == 5 ~ 'C',
      division == 6 ~ 'EC',
      division == 7 ~ 'SW',
      division == 8 ~ 'SC',
      TRUE ~ 'SE'
    )
  )
counties

ggplot() +
  geom_sf(data = counties, aes(fill = division_name), alpha = 0.7) +
  scale_fill_viridis_d() +
  theme_minimal()
