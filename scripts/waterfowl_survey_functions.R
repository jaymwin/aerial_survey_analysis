
# create new database (starting in 2021 only)
create_new_databases <- function(...) {
  
  # create database folder first; this is created just once
  dir_create(here::here('databases'))
  
  # create waterfowl counts table 
  waterfowl_counts <- read_csv(
    here::here('raw_data/archived_survey_data/Fowler_wsds73_19.csv'), 
    # set col_types because some data is read in incorrectly
    # all except direction, side can be treated as numeric
    col_types = cols(
      .default = "d", 
      DIRECT = 'c', # direction of transect
      SIDE = 'c' # side of plane
    )
  ) %>%
    clean_names() # standardize column headings
  # glimpse(waterfowl_counts)
  
  # next, create aerial wetland count table
  air_wet <- read_csv(here::here("raw_data/archived_survey_data/AIRWET7321.csv")) %>%
    clean_names() %>%
    mutate(
      # create region variable based off transect number
      region = case_when(
        transect < 30 ~ 1, # SEC
        transect >= 30 & transect < 43 ~ 2, # NHI
        transect >= 43 & transect < 61 ~ 3, # NLO
        transect >= 61 & transect < 72 ~ 4 # SWD
      )
    ) %>%
    filter(year != 2021)
  # glimpse(air_wet)
  
  # now create the empty sqlite database and populate it with raw duck and wetland count tables
  hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))
  dbWriteTable(hist_db, "raw_duck_counts", waterfowl_counts) # create duck count table
  dbWriteTable(hist_db, "raw_wetland_counts", air_wet) # create wetland count table
  
  # check that worked
  # src_dbi(hist_db)
  
  # and lazily load data
  # tbl(hist_db, "raw_duck_counts")
  # tbl(hist_db, "raw_wetland_counts")
  
  
  # also create tables for vcf-corrected estimates (plus swans)
  
  # data from Drew; historical data we'll use going forward (1973-2019)
  State_DuckSum <- read_csv(here::here('raw_data/archived_survey_data/State_DuckSum.csv'))
  swans <- read_csv(here::here('raw_data/archived_survey_data/State_TSwan_AnnualEst.csv'))
  
  # priority waterfowl species
  ducks_geese <- State_DuckSum %>%
    janitor::clean_names() %>%
    # put in long format
    pivot_longer(
      !year, 
      names_to = "species", 
      values_to = "n" # n = VCF-corrected population size across state
    ) %>%
    # standardize names
    mutate(
      species = case_when(
        species == 'bwte' ~ 'blue-winged teal',
        species == 'wodu' ~ 'wood duck',
        species == 'o_ducks' ~ 'other ducks',
        species == 't_ducks' ~ 'total ducks',
        species == 'geese' ~ 'canada goose',
        TRUE ~ 'mallard'
      )
    ) %>%
    arrange(species, year) %>%
    filter(year != 2021)
  # glimpse(ducks_geese)
  
  # trumpeter swans
  swans <- swans %>%
    janitor::clean_names() %>%
    rename(n = tswans_survey_est) %>% # for swans, n is not VCF-corrected
    mutate(
      species = 'trumpeter swan',
      n = na_if(n, '.'),
      n = as.numeric(n)
    ) %>%
    select(year, species, n) %>%
    filter(year != 2021)
  # glimpse(swans)
  
  # save to database
  dbWriteTable(hist_db, "vcf_corrected_pop_estimates", ducks_geese) # vcf-corrected waterfowl count table
  dbWriteTable(hist_db, "swan_pop_estimates", swans) # swan count table
  
  # check that worked
  # src_dbi(hist_db)
  
  # and lazily load data
  # tbl(hist_db, "vcf_corrected_pop_estimates")
  # tbl(hist_db, "swan_pop_estimates")
  
  
  # add in transect, species metadata
  transect_metadata <- read_csv(here::here('raw_data/archived_survey_data/transect_metadata.csv'))
  species_codes <- read_csv(here::here('raw_data/archived_survey_data/waterfowl_spp_codes.csv'))
  
  hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))
  dbWriteTable(hist_db, "transects_regions", transect_metadata) # create duck count table
  dbWriteTable(hist_db, "species_codes", species_codes) # create duck count table
  
  # and always disconnect from database when finished
  dbDisconnect(hist_db)
  
  
  # create geopackage (for spatial data)
  
  # for now, just the aerial transects
  air_transects <- read_sf(here::here('raw_data/archived_survey_data/WSDSairRtsWTM91.shp'))
  
  # save transect sf object to new geopackage
  air_transects %>%
    st_write(
      dsn = here::here('databases/survey_gis.gpkg'),
      layer = 'aerial_transects'
    )
  
  # pull in wi state border
  wi <- ne_states(country = 'united states of america', returnclass = 'sf') %>%
    filter(postal == 'WI')
  
  # save wi border sf object to geopackage
  wi %>%
    st_write(
      dsn = here::here('databases/survey_gis.gpkg'),
      layer = 'wi_border'
    )
  
  # check this worked
  # st_layers(here::here('databases/survey_gis.gpkg'))
  
}

# function to append analysis year data into database
update_database <- function(db, table, data_to_append) {
  
  # collect data from db
  df <- tbl(db, table) %>%
    collect()
  
  # add some flexibility here if year is in 2-digit format
  year_length <- df %>%
    slice(1) %>%
    select(year) %>%
    mutate(year_length = str_count(year)) %>%
    pull(year_length)

  # change to 4 for determining max year of data
  if (year_length == 2) {
    
    df <- df %>%
      mutate(
        year = case_when(
          # year > 21 ~ year + 1900, # pre-2000 years ### THIS COULD BE A PROBLEM IN >2022 SO REMOVED ###
          year >= 73 & year <= 99 ~ year + 1900, # pre-2000 years
          TRUE ~ year + 2000 # >= 2000
        )
      )
    
  }
  
  # pull last year of data from sqlite database
  most_recent_db_year <- df %>%
    filter(year == max(year)) %>%
    distinct(year) %>%
    pull(year)
  
  # now determine if analysis year data is already in the database
  if (most_recent_db_year == analysis_year) {
    
    # don'ta append data
    message(str_glue("data not appended, database already updated for {analysis_year}!"))
    
  } else {
    
    # append data
    print(str_glue('append data for {analysis_year}...'))
    DBI::dbWriteTable(db, table, data_to_append, append = TRUE)
    
  }
  
}


# function to summarize wetland counts
summarize_wetlands <- function(x, y) { 
  
  # start with unoccupied wetlands
  unoccupied <- x %>%
    group_by(year, region) %>%
    summarise(across(typ1:dit, ~sum(.x, na.rm = TRUE))) %>%
    rename(ditch = dit, stream = str)
  
  # then occupied
  occupied <- y %>%
    group_by(year, region) %>%
    count(wetype) %>%
    spread(wetype, n) %>%
    mutate(
      across(everything(), ~replace_na(.x, 0)) # replace na counts with 0
    ) %>%
    select(-`10`) # get rid of `10` column; these are fields
  
  # give each of these data frames the same name
  occupied <- occupied %>%
    select(year, region, typ1 = `1`, typ2 = `2`, typ3 = `3`, typ4 = `4`, 
           typ5 = `5`, typ6 = `6`, typ7 = `7`, typ8 = `8`, stream = `9`, ditch = `0`)
  
  # wetland types (count)
  total_wetlands <- unoccupied %>%
    mutate(across(typ1:ditch, ~.x * 2)) %>% # multiply by 2 for 2 sides of plane?
    mutate(type = 'unoccupied') %>% # this helps for summing occupied and unoccupied later
    bind_rows(., occupied) %>%
    group_by(year, region) %>%
    summarise(across(typ1:ditch, ~sum(.x, na.rm = TRUE))) %>% # sum by wetland type
    ungroup()
  
  # now calculate wetlands per sqm for particular regions
  wet_psqm <- total_wetlands %>%
    split(.$region)
  
  # this could probably be one function
  wet_psqm$`1` <- wet_psqm$`1` %>%
    mutate(across(typ1:ditch, ~ (.x /29) / 7.5)) #
  
  wet_psqm$`2` <- wet_psqm$`2` %>%
    mutate(across(typ1:ditch, ~ (.x /13) / 7.5)) #
  
  wet_psqm$`3` <- wet_psqm$`3` %>%
    mutate(across(typ1:ditch, ~ (.x /13) / 7.5)) #
  
  wet_psqm$`4` <- wet_psqm$`4` %>%
    mutate(across(typ1:ditch, ~ (.x /11) / 7.5)) # 
  
  # combine regions
  wet_psqm <- wet_psqm$`1` %>%
    bind_rows(., wet_psqm$`2`, wet_psqm$`3`, wet_psqm$`4`) %>%
    arrange(year, region)
  
  # wetland classes summed by different types
  sum_wet_psqm <- wet_psqm %>%
    group_by(year, region) %>%
    summarize(
      i = sum(c(typ1, typ2, typ6)), #
      ii = sum(typ3), #
      iii = sum(c(typ4, typ5)), # 
      iv = sum(c(typ7, typ8)), # 
      non_linear = sum(c(typ1, typ2, typ3, typ4, typ5, typ6, typ7, typ8)), # 
      stream = sum(stream), # stream
      ditch = sum(ditch), # ditch
      linear = sum(c(stream, ditch)) # linear wetlands
    ) %>%
    ungroup()
  
  # combine summaries in a list
  results <- list(occupied, unoccupied, total_wetlands, wet_psqm, sum_wet_psqm)
  return(results)
  
}


# function to estimate VCF, automatically determines correct number of
# survey years for each species that results in CV < 0.20
calculate_vcf <- function(df, spp) {
  
  for(i in 2:length(unique(df$year))) { # do we always need at least 2 years of data?
    
    # work backwards from current year
    all_years <- unique(df$year) %>%
      as_tibble() %>%
      arrange(value)
    
    # go back i years to try to calculate vcf
    vcf_years <- all_years %>%
      slice_tail(n = i) %>%
      pull(value)
    
    # now filter data within i years and by species
    df_year_spp <- df %>%
      filter(year %in% c(vcf_years) & p_species == spp)
    
    # begin Drew's calculations
    # create transect by year variable for grouping and summarizing air and ground counts
    df_year_spp <- df_year_spp %>%
      pivot_wider(names_from = grd, values_from = ind_birds_R) %>%
      rename(air = `1`, grd = `2`) %>%
      mutate(year_x_transect = as.factor(paste(year, transect)))
    
    # sum air and ground counts by transect/year
    df_air_to_ground <- df_year_spp %>%
      group_by(year_x_transect) %>%
      summarise(
        air = sum(air, na.rm = TRUE), 
        grd = sum(grd, na.rm = TRUE)
        ) %>%
      mutate(
        sqx = air^2,
        sqy = grd^2,
        xy = air * grd
      )
    
    # set number of transects in calculations below
    # this number changes with the number of years considered
    n_transects <- 27 * i # adjust number of transects
    
    # perform VCF calculations
    vcf_calc <- tibble(
      species = spp, # priority species code
      sum_sqx = sum(df_air_to_ground$sqx),
      sum_sqy = sum(df_air_to_ground$sqy),
      sum_xy = sum(df_air_to_ground$xy),
      sum_air = sum(df_air_to_ground$air),
      sum_grd = sum(df_air_to_ground$grd),
      r = sum_grd / sum_air,
      var_top = sum_sqy + ((r^2) * sum_sqx) - (2 * r * sum_xy), 
      var_bottom = ((sum_air / n_transects)^2) * (n_transects * (n_transects - 1)), 
      var_r = var_top / var_bottom, 
      se_r = sqrt(var_r), 
      cvr_r = se_r / r
    )
    
    # gather results
    results <- list(df_year_spp, df_air_to_ground, vcf_calc)
    
    # VCF results here (vcf_calc from list)
    res <- results[[3]] %>% 
      as_tibble() %>%
      mutate(
        min_year = min(df_year_spp$year), # first year of VCF calculations
        max_year = max(df_year_spp$year), # last year (current year) of VCF calculations
        total_years = max_year - min_year # total years needed
      )
    
    # CV of VCF
    cv <- results[[3]]$cvr_r
    
    # stop looping through years if VCF CV < 0.20
    if(cv < 0.20) {
      break
    }
    
  }
  
  return(res) # save results here where CV < 0.20
  
}


# function to estimate population size (by region)
estimate_population_size_by_region <- function(count_df, vcf_df, species_code, region_num) {
  
  # filter count data by species, region
  # sum count in each transect
  df <- count_df %>%
    filter(p_species == species_code & region == region_num) %>%
    group_by(transect) %>%
    summarize(total_transect = sum(ind_birds_Pop))

  # number of transects and area by region
  # needed for b * a * r equation
  survey_parameters <- tibble(
    region = seq(1, 4, 1),
    transects = c(29, 13, 13, 11),
    area = c(17949, 9431, 15979, 12311)
  )
  
  # estimates are based on region-specific parameters, filter here
  # grab number of transects
  transects <- survey_parameters %>%
    filter(region == region_num) %>%
    pull(transects)
  
  # grab region-specific area
  area <- survey_parameters %>%
    filter(region == region_num) %>%
    pull(area)
  
  # also subset VCF dataframe to species of interest
  VCF <- vcf_df %>%
    filter(species == species_code)

  # now estimate population size using VCF and aerial counts
  pop_calc <- tibble(
    species = species_code,
    region = region_num,
    total_air = sum(df$total_transect),
    b = (total_air / transects) / 7.5,
    sqm = 56.25,
    sum_sq_air = sum((df$total_transect)^2),
    sum_sq = 56.25 * transects,
    sum_air_xm = total_air * 7.5,
    var_b = (sum_sq_air + ((b^2) * (sum_sq)) - (2 * b * (sum_air_xm))) / (sqm * transects * (transects - 1)),
    se_b = sqrt(var_b),
    cv_b = se_b / b,
    area = area,
    pop_est = b * area * VCF$r,
    pop_var = (area^2) * (((var_b) * (VCF$r^2)) + (VCF$var_r * (b^2)) - (var_b * VCF$var_r)),
    pop_se_1 = sqrt(pop_var),
    r = VCF$r,
    pop_se_2 = sqrt(pop_var) / sqrt(transects)
  )

  # collect results
  results <- list(VCF, pop_calc) # VCF and population estimates in list form
  results <- results[2] %>% # really just interested in population estimates (?)
    as.data.frame() %>%
    as_tibble()
  
}


# function to estimate population size (just swans)
estimate_population_size_swans <- function(count_df, region_num) {
  
  # again filter count data, but only to specific region as we'll start with a swan-only
  # data set
  # sum counts for each transect
  df <- count_df %>%
    filter(region == region_num) %>%
    group_by(transect) %>%
    summarize(total_transect = sum(ind_birds_Pop))
  
  # number of transects and area by region
  survey_parameters <- tibble(
    region = seq(1, 4, 1),
    transects = c(29, 13, 13, 11),
    area = c(17949, 9431, 15979, 12311)
  )
  
  # estimates are based on region-specific parameters, filter here
  # grab number of transects
  transects <- survey_parameters %>%
    filter(region == region_num) %>%
    pull(transects)
  
  # grab region-specific area
  area <- survey_parameters %>%
    filter(region == region_num) %>%
    pull(area)
  
  # estimate population size
  pop_calc <- tibble(
    species = 81, # 81 for tundra swan
    region = region_num,
    total_air = sum(df$total_transect),
    b = (total_air / transects) / 7.5,
    sqm = 56.25,
    sum_sq_air = sum((df$total_transect)^2), 
    sum_sq = 56.25 * transects, 
    sum_air_xm = sum((df$total_transect * 7.5)), 
    var_b = (sum_sq_air + ((b^2) * (sum_sq)) - (2 * b * (sum_air_xm))) / (sqm * transects * (transects - 1)), 
    se_b = sqrt(var_b) ,
    cv_b = se_b / b,
    area = area,
    pop_est = b * area,
    pop_var = (area^2)*(((var_b))),
    pop_se_1 = sqrt(pop_var),
    pop_se_2 = sqrt(pop_var) / sqrt(transects)   
  )
  
  return(pop_calc)
  
}


# function to estimate population size (statewide)
estimate_population_size_statewide <- function(count_df, vcf_df, species_code) {
  
  # filter count data for specific species, all regions
  # sum for each transect
  df <- count_df %>%
    filter(p_species == species_code) %>%
    group_by(transect) %>%
    summarize(total_transect = sum(ind_birds_Pop))
  
  # subset VCF dataframe to species of interest
  VCF <- vcf_df %>%
    filter(species == species_code)
  
  # now estimate population size using VCF and aerial counts
  pop_calc <- tibble(
    species = species_code,
    total_air = sum(df$total_transect),
    b = (total_air / 66) / 7.5,
    sqm = 56.25,
    sum_sq_air = sum((df$total_transect)^2),
    sum_sq = 56.25 * 66,
    sum_air_xm = total_air * 7.5,
    var_b = (sum_sq_air + ((b^2) * (sum_sq)) - (2 * b * (sum_air_xm))) / (sqm * 66 * (66 - 1)),
    se_b = sqrt(var_b),
    cv_b = se_b / b,
    area = 55670,
    pop_est = b * area * VCF$r,
    pop_var = (area^2) * (((var_b) * (VCF$r^2)) + (VCF$var_r * (b^2)) - (var_b * VCF$var_r)),
    pop_se_1 = sqrt(pop_var),
    r = VCF$r,
    pop_se_2 = sqrt(pop_var) / sqrt(66)
  )
  
  results <- list(VCF, pop_calc)
  results <- results[2] %>% # really just interested in population estimates (?)
    as.data.frame() %>%
    as_tibble()
  
}


# run analysis scripts all together once we have new survey data
# end product could be something like this...
analyze_survey_data <- function(analysis_year) {
  
  usethis::ui_todo("Creating historical database (for 2021 only)...")
  ddpcr::quiet(source(here::here('scripts/00-create-databases.R'))) # silence code output with quiet()
  
  usethis::ui_todo("Importing/cleaning/appending data for {analysis_year}...")
  ddpcr::quiet(source(here::here('scripts/01-import_new_survey_data.R'))) # silence code output with quiet()
  
  usethis::ui_todo("Summarizing waterfowl counts for {analysis_year}...")
  ddpcr::quiet(source(here::here('scripts/02-summarize_duck_counts.R')))

  usethis::ui_todo("Summarizing wetland counts for {analysis_year}...")
  ddpcr::quiet(source(here::here('scripts/03-summarize_wetland_counts.R')))
  
  usethis::ui_todo("Calculating visibility correction factors/corrected population sizes for {analysis_year}...")
  ddpcr::quiet(source(here::here('scripts/04-estimate_vcf_pop_sizes.R')))
  
  usethis::ui_todo("Fitting state-space models for {analysis_year}...")
  ddpcr::quiet(source(here::here('scripts/05-fit_state_space_models.R')))
  
  usethis::ui_done("{analysis_year} analysis complete!")
  
}


# create a new, year-specific report. manually modify this each year
create_new_report_rmd <- function(analysis_year) {
  
  file_copy(
    path = here::here('scripts/rmarkdown/oas_report.Rmd'), 
    new_path = str_c(here::here('scripts/rmarkdown/oas_report'), '_', analysis_year, '.Rmd')
    )
  
}
