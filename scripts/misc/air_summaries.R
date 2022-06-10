

count_by_transect <- ws_structured_air %>%
  group_by(transect, species, year) %>%
  summarise(count = sum(ind_birds_Pop)) %>%
  arrange(year, transect, species)
count_by_transect

count_by_transect <- count_by_transect %>%
  filter(species %in% c(50, 32, 40, 44)) %>%
  left_join(., spp_codes)
count_by_transect

count_by_transect <- count_by_transect %>%
  group_by(transect, common_name) %>%
  summarise(count = mean(count))
count_by_transect


library(raster)

r <- raster(file.choose())
r

plot(r)

count_by_transect
library(sf)

# read in aerial transects shapefile
air_rts <- read_sf(here::here('databases/survey_gis.gpkg'), layer = 'aerial_transects') %>%
  st_transform(., 4326) %>% 
  janitor::clean_names() %>%
  group_by(transect) %>%
  summarise() # union transects to help clean up plotting later

# pull in WI boundary
wi <- read_sf(here::here('databases/survey_gis.gpkg'), layer = 'wi_border')

# create region variable based off transect number
air_rts <- air_rts %>%
  mutate(
    region = case_when(
      transect < 30 ~ 'SEC', # SEC = 1; transects 1-29
      transect >= 30 & transect < 43 ~ 'NHI', # NHI = 2; transects 30-42
      transect >= 43 & transect < 61 ~ 'NLO', # NLO = 3; transects 43-60
      transect >= 61 & transect < 72 ~ 'SWD' # SWD = 4; transects 61-71
    )
  )
air_rts

air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Ring-necked Duck') %>%
  ggplot() +
  geom_sf(data = wi) +
  geom_sf(aes(size = count), show.legend = "line") +
  # geom_sf_text(data = air_rts, aes(label = transect), colour = "black") +
  theme_minimal() +
  scale_color_viridis_d(name = 'Region') +
  theme(axis.title = element_blank()) +
  ggtitle(str_glue('Ring-necked Duck'))

air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Blue-winged Teal') %>%
  ggplot() +
  geom_sf(data = wi) +
  geom_sf(aes(size = count), show.legend = "line") +
  # geom_sf_text(data = air_rts, aes(label = transect), colour = "black") +
  theme_minimal() +
  scale_color_viridis_d(name = 'Region') +
  theme(axis.title = element_blank()) +
  ggtitle(str_glue('Blue-winged Teal'))

air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Wood Duck') %>%
  ggplot() +
  geom_sf(data = wi) +
  geom_sf(aes(size = count), show.legend = "line") +
  # geom_sf_text(data = air_rts, aes(label = transect), colour = "black") +
  theme_minimal() +
  scale_color_viridis_d(name = 'Region') +
  theme(axis.title = element_blank()) +
  ggtitle(str_glue('Wood Duck'))

air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Mallard') %>%
  ggplot() +
  geom_sf(data = wi) +
  geom_sf(aes(size = count), show.legend = "line") +
  # geom_sf_text(data = air_rts, aes(label = transect), colour = "black") +
  theme_minimal() +
  scale_color_viridis_d(name = 'Region') +
  theme(axis.title = element_blank()) +
  ggtitle(str_glue('Mallard'))

df <- air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Blue-winged Teal')
df

df <- df %>%
  st_transform(., st_crs(r))
df

library(stars)
ggplot() +
  geom_stars(data = r %>% st_as_stars) +
  geom_sf(data = df, aes(size = count), color = 'white', fill = NA, pch = 21) +
  scale_fill_viridis_c(na.value = NA) +
  guides(fill = 'none', size = 'none') + 
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave(here::here('air_vs_ebird.png'), width = 5, height = 5, units = 'in')


air_rts
plot(air_rts)

buff_transects <- air_rts %>%
  st_transform(., st_crs(r)) %>%
  st_buffer(., 201.168, endCapStyle = "FLAT") 
buff_transects

library(exactextractr)

buff_transects$ebird <- exact_extract(r, buff_transects, 'mean')
buff_transects

buff_transects %>%
  st_drop_geometry() %>%
  left_join(., df) %>%
  ggplot() +
  geom_point(aes(count, ebird))

buff_transects %>%
  st_drop_geometry() %>%
  left_join(., df) -> buff_transects

buff_transects <- buff_transects %>% drop_na()
  
cor(buff_transects$ebird, buff_transects$count)

library(brms)

m1 <- brm(data = buff_transects, 
    family = student,
    bf(mvbind(ebird, count) ~ 1) + set_rescor(TRUE),
    backend = 'cmdstanr',
    iter = 2000, warmup = 500, chains = 4, cores = 4, 
    seed = 210191)
summary(m1)

m <- brm(data = buff_transects, 
         family = student,
         ebird ~ count,
         backend = 'cmdstanr',
         iter = 2000, warmup = 500, chains = 4, cores = 4, 
         seed = 210191
)
bayes_R2(m) # 0.46


library(tidybayes)
p1 <- m1 %>%
  posterior_samples() %>%
  select(rescor__ebird__count) %>%
  ggplot() +
  geom_halfeyeh(aes(rescor__ebird__count)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p1
r2 <- raster(file.choose())
plot(r2)

df <- air_rts %>%
  st_centroid() %>%
  left_join(., count_by_transect) %>%
  filter(common_name == 'Blue-winged Teal')
df

df <- df %>%
  st_transform(., st_crs(r2))
df

library(stars)
ggplot() +
  geom_stars(data = r2 %>% st_as_stars) +
  geom_sf(data = df, aes(size = count), color = 'white', fill = NA, pch = 21) +
  scale_fill_viridis_c(na.value = NA) +
  guides(fill = 'none', size = 'none') + 
  theme_minimal()
ggsave(here::here('air_vs_ebird.png'), width = 5, height = 5, units = 'in')


air_rts
plot(air_rts)

buff_transects <- air_rts %>%
  st_transform(., st_crs(r2)) %>%
  st_buffer(., 201.168, endCapStyle = "FLAT") 
buff_transects

library(exactextractr)

buff_transects$ebird <- exact_extract(r2, buff_transects, 'mean')
buff_transects

buff_transects %>%
  st_drop_geometry() %>%
  left_join(., df) %>%
  ggplot() +
  geom_point(aes(count, ebird))

buff_transects %>%
  st_drop_geometry() %>%
  left_join(., df) -> buff_transects

buff_transects <- buff_transects %>% drop_na()

cor(buff_transects$ebird, buff_transects$count)

library(brms)

m2 <- brm(data = buff_transects, 
          family = student,
          bf(mvbind(ebird, count) ~ 1) + set_rescor(TRUE),
          backend = 'cmdstanr',
          iter = 2000, warmup = 500, chains = 4, cores = 4, 
          seed = 210191)
summary(m2)

library(tidybayes)
p2 <- m2 %>%
  posterior_samples() %>%
  select(rescor__ebird__count) %>%
  ggplot() +
  geom_halfeyeh(aes(rescor__ebird__count)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

library(patchwork)

p1 + p2

m2 %>%
  posterior_samples() %>%
  select(rescor__ebird__count) %>%
  mutate(type = 'expert') %>%
  bind_rows(., 
            m1 %>%
              posterior_samples() %>%
              select(rescor__ebird__count) %>%
              mutate(type = 'ebird')) %>%
  ggplot() +
  stat_halfeyeh(aes(rescor__ebird__count, color = type, fill = type), alpha = 0.5) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

m <- brm(data = buff_transects, 
family = student,
ebird ~ count,
backend = 'cmdstanr',
iter = 2000, warmup = 500, chains = 4, cores = 4, 
seed = 210191
)
bayes_R2(m) # 0.54
