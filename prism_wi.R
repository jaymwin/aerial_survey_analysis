
library(prism)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(raster)
library(tmap)

select <- dplyr::select

# analysis_year <- lubridate::today() %>% lubridate::year() - 1


# climate normals ---------------------------------------------------------

# download prism data here
prism_set_dl_dir(str_c(here::here('output'), '/', analysis_year, '/', 'prism'))

# download precip normals (1991-2020)
get_prism_normals(
  type = "ppt", # these are in monthly totals
  resolution = "4km", 
  mon = c(10, 11, 12, 1, 2, 3, 4, 5), # winter/spring
  keepZip = FALSE
  )

# view files
prism_archive_ls()

# view file paths
pd_to_file(prism_archive_ls())

# subset to winter months
ppt_winter_normal <- prism_archive_subset(
  "ppt", # these are in monthly totals
  "monthly normals", 
  mon = c(10, 11, 12, 1, 2), 
  resolution = "4km"
)

# subset to spring months
ppt_spring_normal <- prism_archive_subset(
  "ppt", 
  "monthly normals", 
  mon = c(3, 4, 5), 
  resolution = "4km"
)

# convert to raster; sum across winter months = precip total
ppt_winter_normal <- pd_to_file(ppt_winter_normal)
ppt_winter_normal_ras <- stack(ppt_winter_normal)
ppt_winter_normal_ras <- calc(ppt_winter_normal_ras, sum) # total

# convert to raster; sum across spring months = precip total
ppt_spring_normal <- pd_to_file(ppt_spring_normal)
ppt_spring_normal_ras <- stack(ppt_spring_normal)
ppt_spring_normal_ras <- calc(ppt_spring_normal_ras, sum)

# pull in wi border
wi_border <- ne_states(country = 'united states of america', returnclass = 'sf') %>%
  filter(postal == 'WI') %>%
  select(geometry) %>%
  st_transform(., st_crs(ppt_winter_normal_ras))

# crop US-wide prism data to WI border
ppt_winter_normal_ras <- ppt_winter_normal_ras %>%
  crop(., wi_border) %>%
  mask(., wi_border)

ppt_spring_normal_ras <- ppt_spring_normal_ras %>%
  crop(., wi_border) %>%
  mask(., wi_border)
  
# view normals; winter
tm_shape(ppt_winter_normal_ras) +
  tm_raster(title = "Precipitation (mm):", style = "cont", palette = "viridis") +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black')

# spring
tm_shape(ppt_spring_normal_ras) +
  tm_raster(title = "Precipitation (mm):", style = "cont", palette = "viridis") +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black')


# analysis year -----------------------------------------------------------

# download current year precipitation (technically current year, current year - 1)
get_prism_monthlys(
  type = "ppt", # these are in monthly totals
  year = c(analysis_year, analysis_year - 1),
  mon = c(10, 11, 12, 1, 2, 3, 4, 5), # winter, spring
  keepZip = FALSE
  )

# subset to winter (so for 2021, Oct. 2020 - Feb 2021)
ppt_winter_analysis_year <- prism_archive_subset(
  "ppt", 
  "monthly", 
  years = c(analysis_year, analysis_year - 1),
  mon = c(10, 11, 12, 1, 2)
) %>%
  as_tibble() %>%
  slice(6:10) %>%
  pull(value)

# subset to spring (so for 2021, Mar 2021 - May 2021)
ppt_spring_analysis_year <- prism_archive_subset(
  "ppt", 
  "monthly", 
  years = c(analysis_year, analysis_year - 1),
  mon = 3:5
) %>%
  as_tibble() %>%
  slice(4:6) %>%
  pull(value)

# convert to raster; sum across winter months = precip total
ppt_winter_analysis_year <- pd_to_file(ppt_winter_analysis_year)
ppt_winter_analysis_year_ras <- stack(ppt_winter_analysis_year)
ppt_winter_analysis_year_ras <- calc(ppt_winter_analysis_year_ras, sum)

# convert to raster; sum across spring months = precip total
ppt_spring_analysis_year <- pd_to_file(ppt_spring_analysis_year)
ppt_spring_analysis_year_ras <- stack(ppt_spring_analysis_year)
ppt_spring_analysis_year_ras <- calc(ppt_spring_analysis_year_ras, sum)

# crop US-wide prism data to WI border
ppt_winter_analysis_year_ras <- ppt_winter_analysis_year_ras %>%
  crop(., wi_border) %>%
  mask(., wi_border)

ppt_spring_analysis_year_ras <- ppt_spring_analysis_year_ras %>%
  crop(., wi_border) %>%
  mask(., wi_border)

# view analysis year; winter
tm_shape(ppt_winter_analysis_year_ras) +
  tm_raster(title = "Precipitation (mm):", style = "cont", palette = "viridis") +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black')

# view analysis year; spring
tm_shape(ppt_spring_analysis_year_ras) +
  tm_raster(title = "Precipitation (mm):", style = "cont", palette = "viridis") +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black')


# anomalies ---------------------------------------------------------------

# function to subtract current year by 30-year average
calculate_anomalies <- function(x, y) {
  return(x - y)
}

# winter anomaly
anom_ras_winter <- raster::overlay(
  ppt_winter_analysis_year_ras, 
  ppt_winter_normal_ras, 
  fun = calculate_anomalies
  )

# spring anomaly
anom_ras_spring <- raster::overlay(
  ppt_spring_analysis_year_ras, 
  ppt_spring_normal_ras, 
  fun = calculate_anomalies
  )


# summarize data ----------------------------------------------------------

# plot anomalies; winter
p1 <- tm_shape(anom_ras_winter) +
  tm_raster(title = "Precip. \nanom. (mm):", style = "cont", palette = "RdYlGn", legend.reverse = TRUE) +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black') +
  tm_layout(main.title = "Winter (Oct-Feb)", legend.position = c("left", "bottom"))

# spring
p2 <- tm_shape(anom_ras_spring) +
  tm_raster(title = "Precip. \nanom. (mm):", style = "cont", palette = "RdYlGn", legend.reverse = TRUE) +
  tm_shape(wi_border) +
  tm_borders(lwd = 2, col = 'black') +
  tm_layout(main.title = "Spring (Mar-May)", legend.position = c("left", "bottom"))

tm <- tmap_arrange(p1, p2)
tm

# save
tmap_save(tm, str_c(here::here('output'), '/', analysis_year, '/', 'prism/precip_anomalies.png'), width = 7.5, height = 4, dpi = 600, units = 'in')

tibble(
  statewide_winter_analysis_year_mean = mean(values(ppt_winter_analysis_year_ras), na.rm = TRUE),
  statewide_spring_analysis_year_mean = mean(values(ppt_spring_analysis_year_ras), na.rm = TRUE)
)

tmaptools::palette_explorer()

mean(values(ppt_winter_analysis_year_ras), na.rm = TRUE)
mean(values(ppt_spring_analysis_year_ras), na.rm = TRUE)

mean(values(ppt_winter_normal_ras), na.rm = TRUE)
mean(values(ppt_spring_normal_ras), na.rm = TRUE)

