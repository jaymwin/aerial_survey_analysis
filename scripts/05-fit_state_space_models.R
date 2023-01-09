
# fit state-space models in JAGS for each species (or group - total ducks, other ducks)

# load libraries ----------------------------------------------------------

library(jagsUI)
library(MCMCvis)
library(tidyverse)
library(fs)
library(DBI)
library(dbplyr)


# load data ---------------------------------------------------------------

# pull in VCF-corrected population estimates
# the database contains current year estimates at this point
hist_db <- dbConnect(RSQLite::SQLite(), here::here("databases/historical_db.sqlite"))
src_dbi(hist_db)

# ducks and geese estimates (VCF-corrected)
ducks_geese <- tbl(hist_db, 'vcf_corrected_pop_estimates') %>%
  collect()

# swans estimates (not VCF-corrected)
swans <- tbl(hist_db, 'swan_pop_estimates') %>%
  collect()

# disconnect
dbDisconnect(hist_db)

# combine all species now for state-space modeling
waterfowl_pop_estimates <- ducks_geese %>%
  bind_rows(., swans)

# create a year-specific directory to store fitted JAGS models
dir_create(str_c(here::here('output'), '/', analysis_year, '/', 'fitted_models'))


# run state-space models for each species/group  --------------------------

# this fits a simple model with density independent population growth in JAGS
# extract unique species and groups analyzing count trends
spp <- unique(waterfowl_pop_estimates$species)

set.seed(608)

# create an empty list to store annual state-space predictions for each species/group
jags_output_all_species <- c()

# loop through each species
for (i in seq_along(spp)) {
  
  # data set for analysis
  df <- waterfowl_pop_estimates %>%
    filter(species == spp[i]) %>% # filter to species
    filter( # remove early NA years for geese, swans
      case_when(
        species == 'canada goose' ~ year >= 1986, # 1986-onward
        species == 'trumpeter swan' ~ year >= 2005, # 2005-onward
        TRUE ~ year >= 1973 # 1973-onward
      ) 
    )
  
  # extract counts and years for JAGS
  mcounts <- rbind(df$n) # annual counts
  year <- df$year # year
  
  {
    
    # now fit SSM in JAGS for that species
    sink(str_c(here::here('output'), '/', analysis_year, '/', 'fitted_models', '/', spp[i], "_", analysis_year, '.jags'))
    cat("
      model {
      # Specify priors
      for (s in 1:g) { # loop over both species
      start.lnN[s] ~ dnorm(ln1[s], 0.25)  
      lnN[s,1] <- start.lnN[s]  
      alpha[s] ~ dnorm(0, 1) # mean intrinsic growth rate
      epsilon[s] ~ dunif(0, 2) # ann SD in pop growth
      obs.sd[s] ~ dunif(0, 2) 
      # convert SD to precision (tau = 1/SD^2)
      epsilon.tau[s] <- pow(epsilon[s], -2) 
      obs.tau[s] <- pow(obs.sd[s], -2)
      
      # Likelihood
      for (t in 1:(nyr-1)) {
      eps[s,t] ~ dnorm(0, epsilon.tau[s])
      lnN[s,t+1] <- lnN[s,t] + alpha[s] + eps[s,t]}
      
      # Observation process
      for (t in 1:nyr) {
      lnCount[s, t] ~ dnorm(lnN[s, t],obs.tau[s])
      N.est[s, t] <- exp(lnN[s, t]) } # convert log(N) to real scale
      
        } # close s loop
        
      } # end jags model
      ",
        fill = TRUE
        )
    
    sink() # save JAGS model code
    
  }
  
  # bundle data, identify parameters to save, and submit to JAGS
  bbs.data <- list(
    nyr = length(year), # survey years
    lnCount = log(mcounts), # log of annual counts
    ln1 = log(mcounts[,1]), # log of initial count
    g = 1 # number of species
    )
  
  # set parameters
  parameters <- c("alpha", "epsilon", "obs.sd", "N.est")
  
  # fit jags model
  jags_out <- jagsUI(
    bbs.data, # bundled JAGS data
    inits = NULL, 
    parameters, 
    # save species-specific model in output by year
    str_c(here::here('output'), '/', analysis_year, '/', 'fitted_models', '/', spp[i], "_", analysis_year, '.jags'), 
    n.adapt = 1000, 
    n.chains = 3, 
    n.thin = 10, 
    n.iter = 15000, 
    n.burnin = 5000, 
    parallel = TRUE
  )

  # now, pull out parameters of interest (estimated count each year)
  jags_out <- MCMCsummary(jags_out, params = 'N.est') %>%
    as_tibble() %>%
    mutate(species = spp[i]) %>%
    select(
      species, 
      mean, 
      sd, 
      lcl = `2.5%`, 
      ucl = `97.5%`, 
      median = `50%`
      ) %>%
    add_column(year = year)
  
  # save output for each species
  jags_output_all_species[[i]] <- jags_out
  
}

# bind list together of SSM parameters and join with population estimates
jags_output_all_species <- jags_output_all_species %>% 
  bind_rows() %>%
  left_join(., waterfowl_pop_estimates)

# save output for plotting and tables
jags_output_all_species %>%
  write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'state_space_results_', analysis_year, '.csv'))
