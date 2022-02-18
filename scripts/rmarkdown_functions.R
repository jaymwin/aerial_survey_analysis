
library(tidyverse)
library(kableExtra)
library(formattable)
library(sf)
library(rnaturalearth)
library(ggsflabel)
options(knitr.table.format = "latex")
options(tidyverse.quiet = TRUE)

# # function to create report and title by year and type
# render_report <- function(year, report_type) {
#   
#   if (report_type == 'internal') { # for Taylor
#     
#     render(
#       here::here("scripts/rmarkdown/internal_report.Rmd"), # .rmd file for internal report
#       params = list(
#         year = year # appropriate year to dynamically update title, text
#       ),
#       output_dir = here::here(str_c('reports', '/', year)), # year-specific output directory
#       output_file = str_c("internal_report_", year, ".pdf"), # year-specific report name
#       quiet = TRUE
#     )
#     
#   } else { # final report for Public
#     
#     render(
#       here::here("scripts/rmarkdown/final_report.Rmd"), # .rmd file for final report
#       params = list(
#         year = year # appropriate year to dynamically update title, text
#       ),
#       output_dir = here::here(str_c('reports', '/', year)), # year-specific output directory
#       output_file = str_c("final_report_", year, ".pdf"), # year-specific report name
#       quiet = TRUE
#     )
#     
#   }
#   
# }


# function to create report and title by year and type
render_report <- function(year, report_type) {
  
  if (report_type == 'wildlife_mgt') { # for Taylor
    
    render(
      here::here("scripts/rmarkdown/internal_report.Rmd"), # .rmd file for internal report
      params = list(
        year = year # appropriate year to dynamically update title, text
      ),
      output_dir = here::here(str_c('reports', '/', year)), # year-specific output directory
      output_file = str_c("internal_report_", year, ".pdf"), # year-specific report name
      quiet = TRUE
    )
    
  } else { # OAS final report
    
    render(
      here::here("scripts/rmarkdown/oas_report.Rmd"), # .rmd file for fancier looking report
      params = list(
        year = year # appropriate year to dynamically update title, text
      ),
      output_dir = here::here(str_c('reports', '/', analysis_year)), # year-specific output directory
      output_file = str_c("oas_spring_survey_report_", analysis_year, ".pdf"), # year-specific report name
      quiet = TRUE
    )
    
  }
  
}


# table functions ---------------------------------------------------------

# summarize wetland counts in table form
create_wetland_summary_table <- function(region_code) {
  
  # lay out region codes here; need names for table captions
  regions <- tribble(
    ~region, ~name,
    1, 'SEC',
    2, 'NHI',
    3, 'NLO',
    4, 'SWD'
  )
  
  # filter out region name by region number
  region_name <- regions %>%
    filter(region == region_code) %>% 
    pull(name)
  
  # read in wetland summaries for creating table
  df_wetlands <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # figure out first year or surveys to display in table
  summary_start_year <- df_wetlands %>%
    ungroup() %>%
    select(year) %>%
    distinct() %>%
    slice_tail(n = 10) %>% # slice to last 10 years
    slice(1) %>%
    pull(year) # pull out first year of this 10-year period
  
  # grab last 10 years of surveys from current year
  # note: surveys not done in 2020 due to covid
  annual_wetlands <- df_wetlands %>%
    filter(region == region_code & year >= summary_start_year) %>%
    # change column names for table
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region) %>%
    mutate(across(`I, II, VI`:Linear, ~format(round(.x, 1), nsmall = 1))) %>% # format to 1 decimal place
    ungroup() %>%
    mutate_all(as.character)
  
  # calculate % change from last survey year
  perc_change_last_year <- df_wetlands %>%
    filter(region == region_code & year >= summary_start_year) %>% # filter to region & 10-year period
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region) %>%
    ungroup() %>%
    slice_tail(n = 2) %>% # current year, last year
    mutate(across(`I, II, VI`:Linear, ~(.x/lag(.x) - 1) * 100)) %>% # calculate percent difference
    slice(2) %>% # slice row with percent change
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>% # round to 1 decimal
    mutate(Year = '% Change from previous year') %>%
    mutate(across(`I, II, VI`:Linear, ~str_c(.x, '%'))) # add percent sign
  
  # calculate 10-year mean
  ten_year_mean <- df_wetlands %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>%
    ungroup() %>%
    slice_tail(n = 10) %>% # grab most recent 10-year period
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region)
  
  # extract first, last year for labeling 10-year date span
  min_year <- min(ten_year_mean$Year)
  max_year <- max(ten_year_mean$Year)
  
  # format 10-year mean part of the table
  ten_year_mean <- ten_year_mean %>%
    summarise(across(`I, II, VI`:Linear, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = str_glue('10-Year mean ({min_year}-{max_year})')) %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear) %>%
    mutate(across(everything(), as.character))
  
  # calculate long-term mean (across every year of data)
  long_term_mean <- df_wetlands %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    ungroup() %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>% # all years included in long-term mean
    summarise(across(`I, II, VI`:Linear, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = 'Long-term mean') %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear) %>%
    mutate(across(everything(), as.character))
  
  # calculate current year
  current_year_wetlands <- df_wetlands %>%
    filter(region == region_code & year == analysis_year) %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    filter(region == region) %>%
    ungroup() %>%
    select(-region) %>%
    mutate(Year = as.character(Year))
  
  # calculate long-term mean again to calculate % change from long-term mean
  long_term <- df_wetlands %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    ungroup() %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>%
    summarise(across(`I, II, VI`:Linear, ~mean(.x, na.rm = TRUE))) %>% # includes current year in long-term mean
    mutate(Year = 'Long-term mean') %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear)
  
  # % change from long-term mean
  perc_long_term <- long_term %>%
    bind_rows(., current_year_wetlands) %>%
    mutate(across(`I, II, VI`:Linear, ~(.x/lag(.x) - 1) * 100)) %>%
    slice(2) %>%
    mutate(across(`I, II, VI`:Linear, ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = '% Change from long-term mean') %>%
    mutate(across(`I, II, VI`:Linear, ~str_c(.x, '%')))
  
  # combine dataframes needed for table
  wetland_summary <- perc_change_last_year %>%
    bind_rows(., long_term_mean) %>%
    bind_rows(., perc_long_term) %>%
    bind_rows(., ten_year_mean)
  
  # add annual statistics with percent change summaries
  annual_wetlands <- annual_wetlands %>%
    bind_rows(., wetland_summary)
  
  # create table
  annual_wetlands %>%
    kbl(
      caption = str_glue('Numbers of wetlands per square mile observed during the last 10-year period, {summary_start_year}--{analysis_year}, {region_name} region.'),
      align = str_flatten(rep('c', length(colnames(.)))), # have to center 'c' each column
      booktabs = TRUE, # table formatting
      linesep = "" # prevent spaces every 5th row
    ) %>%
    add_header_above(c(" ", "Wetland type" = 8), bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c('striped', "hold_position", "scale_down")) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    column_spec(1, width = '8em', latex_valign = "m") %>%
    footnote(
      general = "",
      general_title = "Notes: I, II, VI = temporary, wet meadow, and shrub swamp
    wetlands; III, IV = seasonal and semi-permanent wetlands; V =
    permanent/open water wetlands; VII, VIII = wooded swamp and bog wetlands",
      title_format = c("italic"),
      footnote_as_chunk = FALSE,
      threeparttable = TRUE,
      escape = FALSE,
      fixed_small_size = TRUE
    )
  
}


# table of recent trumpeter swan counts
create_trumpeter_table <- function(start_year) {
  
  read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'state_space_results_', analysis_year, '.csv'), show_col_types = FALSE) %>%
    filter(
      species == 'trumpeter swan',
      year >= 2010
      ) %>%
    select(year, survey_estimate = n, model_estimate = mean) %>%
    mutate(across(survey_estimate:model_estimate, ~scales::number(.x, big.mark = ",", accuracy = 1))) %>%
    rename(Year = year, `Survey estimate` = survey_estimate, `Modeled estimate` = model_estimate) %>%
    kbl(
      caption = str_glue('Annual statewide estimates of breeding Trumpeter swan abundance in Wisconsin, {start_year}--{analysis_year}. Raw survey estimates and model-predicted estimates from a Bayesian state-space model are shown. Note that surveys were not conducted in 2020 due to the COVID-19 pandemic.'),
      align = str_flatten(rep('c', length(colnames(.)))), # have to center 'c' each column
      # align = 'ccc',
      booktabs = TRUE,
      linesep = ""
      ) %>%
    add_header_above(c(" ", "Trumpeter swan statewide annual trends" = 2), bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c('striped', 'hold_position')) %>%
    kableExtra::row_spec(0, bold = TRUE)
  
}


# analysis year population size, VCF-related estimates
create_current_breeding_estimate_table <- function() {
  
  df <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_summary', '.csv'), show_col_types = FALSE) %>%
    select(species, region, area, b, r, pop_est, pop_se_1) %>%
    mutate(
      region = case_when(
        region == 1 ~ 'SEC',
        region == 2 ~ 'NHI',
        region == 3 ~ 'NLO',
        region == 4 ~ 'SWD'
      ),
      species = case_when(
        species == 1 ~ 'Mallard',
        species == 2 ~ 'Blue-winged teal',
        species == 3 ~ 'Wood duck',
        species == 4 ~ 'Canada goose',
        TRUE ~ 'Other duck species'
      )
    ) %>%
    mutate(species = fct_relevel(species, 'Mallard', 'Blue-winged teal', 'Wood duck', 'Other duck species', 'Canada goose')) %>%
    arrange(species, region) %>%
    rename(
      "Species" = species, 
      "Stratum" = region, 
      "Area of stratum (mi$^2$)" = area, 
      "Bird density seen from the air (birds/mi$^2$)" = b, 
      `Aerial visibility correction factor` = r, 
      `Survey estimate` = pop_est, 
      `Standard error` = pop_se_1 # pop_se_2 was used by Drew in 2021; very small and raised red flag so using ron's se here now
    ) %>%
    mutate(across("Bird density seen from the air (birds/mi$^2$)":`Aerial visibility correction factor`, ~format(round(.x, 3), nsmall = 3)))
  
  # calculate statewide SEs according to Ron and Drew
  statewide_se <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_summary', '.csv'), show_col_types = FALSE) %>%
    select(species, region, pop_var) %>%
    mutate(
      region = case_when(
        region == 1 ~ 'SEC',
        region == 2 ~ 'NHI',
        region == 3 ~ 'NLO',
        region == 4 ~ 'SWD'
      ),
      species = case_when(
        species == 1 ~ 'Mallard',
        species == 2 ~ 'Blue-winged teal',
        species == 3 ~ 'Wood duck',
        species == 4 ~ 'Canada goose',
        TRUE ~ 'Other duck species'
      )
    ) %>%
    mutate(species = fct_relevel(species, 'Mallard', 'Blue-winged teal', 'Wood duck', 'Other duck species', 'Canada goose')) %>%
    arrange(species, region) %>%
    group_by(species) %>%
    summarise(pop_se_1 = sqrt(sum(pop_var))) %>%
    rename(
      `Standard error` = pop_se_1 # pop_se_2 was used by Drew in 2021; very small and raised red flag so using ron's se here now
    ) %>%
    pull(`Standard error`)

  # calculate subtotals; not sure if actually necessary or what the subtotal is here
  subtotals <- df %>%
    mutate(Stratum = 'Subtotal') %>%
    group_by(Species, Stratum) %>%
    summarize(
      `Survey estimate` = sum(`Survey estimate`)
    ) %>%
    ungroup() %>%
    mutate(`Standard error` = statewide_se)
  
  # now add back and order table by species and strata
  df <- df %>%
    bind_rows(., subtotals) %>%
    mutate(
      Species = fct_relevel(Species, 'Mallard', 'Blue-winged teal', 'Wood duck', 'Other duck species', 'Canada goose'),
      Stratum = fct_relevel(Stratum, 'SEC', 'NHI', 'NLO', 'SWD', 'Subtotal')
    ) %>%
    arrange(Species, Stratum) %>%
    select(-Species) %>%
    mutate(across(c(2, 5:6), ~scales::number(.x, big.mark = ",", accuracy = 1)))
  
  # add footnote markers to strata and aerial visibility columns
  names(df)[c(1,4)] <- paste0(
    names(df)[c(1,4)],
    footnote_marker_symbol(1:2)
  )
  
  # create a special grouped row for other ducks
  prlabel <- paste0(
    "Other duck species",
    footnote_marker_alphabet(1)
  )
  
  # highlight subtotal cells (make font bold)
  df[5, 5] <- cell_spec(df[5, 5], bold = T)
  df[5, 6] <- cell_spec(df[5, 6], bold = T)
  df[10, 5] <- cell_spec(df[10, 5], bold = T)
  df[10, 6] <- cell_spec(df[10, 6], bold = T)
  df[15, 5] <- cell_spec(df[15, 5], bold = T)
  df[15, 6] <- cell_spec(df[15, 6], bold = T)
  df[20, 5] <- cell_spec(df[20, 5], bold = T)
  df[20, 6] <- cell_spec(df[20, 6], bold = T)
  df[25, 5] <- cell_spec(df[25, 5], bold = T)
  df[25, 6] <- cell_spec(df[25, 6], bold = T)
  
  # and need vcfs to update footnote
  vcf_df <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_vcf_estimates', '.csv'), show_col_types = FALSE)
  vcfs <- vcf_df %>% pull(total_years) # years needed to calculate VCF (CV < 0.20) for each species
  
  # create table
  df %>%
    kbl(
      caption = str_glue("Statewide and stratum-specific population estimates for the {analysis_year} Waterfowl Breeding Population Survey population estimates."),
      # align = 'cccccc', # center cell values
      align = str_flatten(rep('c', length(colnames(.)))), # have to center 'c' each column
      escape = FALSE, # needed for latex expressions
      booktabs = TRUE, # table formatting
      linesep = "" # prevent spaces every 5th row
    ) %>%
    pack_rows("Mallard", 1, 5, hline_after = TRUE) %>% # add species grouping rows
    pack_rows("Blue-winged teal", 6, 10, hline_after = TRUE, hline_before = TRUE) %>%
    pack_rows("Wood duck", 11, 15, hline_after = TRUE, hline_before = TRUE) %>%
    pack_rows(prlabel, 16, 20, escape = FALSE, hline_after = TRUE, hline_before = TRUE) %>%
    pack_rows("Canada goose", 21, 25, hline_after = TRUE, hline_before = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position", "scale_down", "striped"), 
      stripe_index = c(1, 3, 5, 6, 8, 10, 11, 13, 15, 16, 18, 20, 21, 23, 25)
    ) %>%
    column_spec(1, width = '10em', latex_valign = "m") %>%
    column_spec(2:6, width = '6em', latex_valign = "m") %>%
    add_indent(c(5, 10, 15, 20, 25), level_of_indent = -7) %>% # subtotal 'indents'
    # add footnote
    footnote(
      footnote_order = c("symbol", "alphabet"),
      symbol = c(
        "\\\\footnotesize{SEC = Southeast Central, NHI = Northern High, NLO = Northern Low, SWD = Southwest Driftless Strata.}",
        str_glue(
          "Aerial Visibility Correction Factor = ratio of number of species-specific individuals seen from the ground divided by the number seen from the air on air-ground segments, pooled across strata. To achieve a desirable coefficient of variation (CV) value in the aerial visibility correction factor, previous years of air-ground data were iteratively added until CV was <20\\\\%. In {analysis_year}, aerial visibility correction factors for mallards, blue-winged teal, wood ducks, Canada geese, and \"other ducks\" were derived using {vcfs[1]}, {vcfs[2]}, {vcfs[3]}, {vcfs[4]}, and {vcfs[5]} years of air ground data, respectively."
        )
      ),
      alphabet = "\\\\footnotesize{Lesser scaup, bufflehead, and all non-duck/goose waterbirds are excluded from analysis. Common duck species categorized as \"other ducks\" include: ring-necked duck, common goldeneye, northern shoveler, hooded merganser, common merganser, gadwall, green-winged teal, and canvasback.}",
      footnote_as_chunk = FALSE,
      threeparttable = TRUE,
      escape = FALSE,
      fixed_small_size = TRUE
    )
  
}


# create table comparing VCF-informed estimates, ssm estimates
create_survey_state_space_estimates_table <- function() {
  
  # read in JAGS SSM output
  jags_output_all_species <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'state_space_results_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # pull in vcf and state-space population estimates
  df <- jags_output_all_species %>%
    filter(species != 'trumpeter swan') %>% # don't need this species for estimates table
    select(year, species, survey_n = n, ssm_n = mean) %>%
      pivot_wider(
        names_from = c(species),
        values_from = c(survey_n, ssm_n),
      ) %>%
    select(year, matches(c('mallard', 'blue', 'wood', 'other', 'total', 'canada')))

  # mean: 1973-current year (all years)
  mean_all_years <- df %>%
    # select(1:13) %>%
    summarize(across(2:13, ~mean(.x, na.rm = TRUE))) %>% # average all years in each column
    mutate(Year = str_glue('Mean (1973--{analysis_year})')) %>%
    select(Year, survey_n_mallard:`ssm_n_canada goose`) %>% # re-order with year first
    mutate(across(c(2:13), ~scales::number(.x, big.mark = ",", accuracy = 1))) # round, format with ,

  # select last 10 years
  min_ten_year <- df %>%
    slice_tail(n = 10) %>%
    select(year) %>%
    min()

  # mean: last 10 years (including current year)
  mean_last_ten_years <- df %>%
    filter(year >= min_ten_year) %>% # grab most recent 10-year period
    # select(1:13) %>%
    summarize(across(2:13, ~mean(.x, na.rm = TRUE))) %>% # average in each column
    mutate(Year = str_glue('Mean ({min_ten_year}--{analysis_year})')) %>%
    select(Year, survey_n_mallard:`ssm_n_canada goose`) %>% # reorder with year first
    mutate(across(c(2:13), ~scales::number(.x, big.mark = ",", accuracy = 1))) # round, format with ,

  # % change from previous year
  percent_change_last_year <- df %>%
    slice_tail(n = 2) %>%
    mutate(across(2:13, ~(.x/lag(.x) - 1) * 100)) %>% # calculate percent difference from last year
    slice(2) %>% # slice row that now contains percent difference
    select(-year) %>% # remove year
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>% # round to 1 decimal place
    mutate(Year = '% change from previous year') %>%
    select(Year, survey_n_mallard:`ssm_n_canada goose`) %>%
    mutate(across(2:13, ~str_c(.x, '%'))) %>% # add percent sign to every value except year
    mutate(across(2:13, ~recode(.x, `NA%` = ""))) # convert NA% to empty cell

  # % change from the mean of 1973 to most recent year - 1 
  # length_of_df <- dim(df)[1] # from report, sounds like long-term includes current year so commented out
  
  # mean of all years
  long_term_mean <- df %>%
    # slice_head(n = length_of_df - 1) %>% 
    summarize(across(2:13, ~mean(.x, na.rm = TRUE)))
  
  # percent change between long-term mean and current year
  percent_change_long_term_mean <- long_term_mean %>% # long-term mean
    bind_rows(., df %>% slice_tail(n = 1) %>% select(-year)) %>% # add on current year
    mutate(across(1:12, ~(.x/lag(.x) - 1) * 100)) %>% # calculate % difference
    slice(2) %>% # slice row containing percent difference
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>% # round to 1 decimal place
    mutate(Year = str_glue('% change from 1973--{analysis_year}')) %>%
    select(Year, survey_n_mallard:`ssm_n_canada goose`) %>%
    mutate(across(2:13, ~str_c(.x, '%')))

  # now combine population estimates and changes
  df <- df %>%
    mutate(across(c(2:13), ~scales::number(.x, big.mark = ",", accuracy = 1))) %>%
    rename(Year = year) %>%
    mutate(Year = as.character(Year)) %>%
    bind_rows(., mean_all_years) %>%
    bind_rows(., mean_last_ten_years) %>%
    bind_rows(., percent_change_last_year) %>%
    bind_rows(., percent_change_long_term_mean)

  # create full table from these
  df %>%
    kbl(
      caption = str_glue("Statewide breeding waterfowl population survey estimates and corresponding Bayesian state-space modeled estimates (highlighted in bold) in Wisconsin, 1973--{analysis_year}."),
      # align = 'ccccccccccccc',
      align = str_flatten(rep('c', length(colnames(.)))), # have to center 'c' each column
      # escape = FALSE, # needed for latex expressions
      booktabs = TRUE, # table formatting
      linesep = "", # prevent spaces every 5th row
      col.names = NULL
    ) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    add_header_above(
      c("Year" = 1,
        "Mallard" = 2,
        "Blue-winged teal" = 2,
        "Wood ducks" = 2,
        "Other ducks" = 2,
        "Total ducks" = 2,
        "Canada geese" = 2
      ),
      bold = TRUE
    ) %>%
    column_spec(c(3, 5, 7, 9, 11, 13), bold = TRUE) %>%
    column_spec(1, width = '7em', latex_valign = "m") %>%
    kableExtra::kable_styling(latex_options = c("repeat_header", "striped", "scale_down", "hold_position"), font_size = 10) # %>%
  # column_spec(1, width = '10em', latex_valign = "m") %>%
  # column_spec(2:6, width = '6em', latex_valign = "m") %>%
  # add_indent(c(5, 10, 15, 20, 25), level_of_indent = -7) %>% # subtotal 'indents'
  # add footnote
  
}


# plotting functions ------------------------------------------------------

# plots that accompany wetland summary tables
# plot_wetland_abundance <- function(region_code) {
#   
#   df_wetlands <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.csv'), show_col_types = FALSE)
#   
#   # lay out region codes here; need names for captions
#   regions <- tribble(
#     ~region, ~name,
#     1, 'Southeast Central Region',
#     2, 'Northern High Density Region',
#     3, 'Northern Low Density Region',
#     4, 'Southwest Driftless Region'
#   )
#   
#   # filter out region name by region number
#   region_name <- regions %>%
#     filter(region == region_code) %>% 
#     pull(name)
#   
#   # plot
#   df_wetlands %>%
#     filter(region == region_code) %>%
#     select(year, non_linear, linear) %>%
#     rename(Year = year, `Non-linear` = non_linear, Linear = linear) %>%
#     gather(wetland_type, wpsqm, -Year) %>%
#     ggplot() +
#     geom_line(aes(Year, wpsqm, color = wetland_type)) +
#     geom_point(aes(Year, wpsqm, color = wetland_type)) +
#     scale_color_brewer(palette = 'Dark2') +
#     labs(
#       y = 'Wetlands per square mile' # ,
#       # title = str_glue('Wetland abundance in Wisconsin {region_name}')
#     ) +
#     guides(color = guide_legend(title = "Wetland type")) +
#     theme_classic() +
#     theme(legend.position = 'top')
#   
# }

# plots that accompany wetland summary tables
plot_wetland_abundance <- function() {
  
  df_wetlands <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # lay out region codes here; need names for captions
  regions <- tribble(
    ~region, ~name,
    1, 'Southeast Central region',
    2, 'Northern High Density region',
    3, 'Northern Low Density region',
    4, 'Southwest Driftless region'
  )
  
  df_wetlands <- df_wetlands %>%
    left_join(., regions, by = 'region') %>%
    mutate(name = fct_reorder(name, region))
  
  # plot
  df_wetlands %>%
    select(year, non_linear, linear, name) %>%
    rename(Year = year, `Non-linear` = non_linear, Linear = linear) %>%
    gather(wetland_type, wpsqm, -Year, -name) %>%
    ggplot() +
    geom_line(aes(Year, wpsqm, color = wetland_type)) +
    geom_point(aes(Year, wpsqm, color = wetland_type)) +
    scale_color_brewer(palette = 'Dark2') +
    labs(
      y = 'Wetlands per square mile' # ,
      # title = str_glue('Wetland abundance in Wisconsin {region_name}')
    ) +
    guides(color = guide_legend(title = "Wetland type")) +
    theme_light() +
    theme(
      legend.position = 'top',
      panel.grid.minor = element_blank()
      ) +
    facet_wrap(~name, scales = 'free')
  
}


# plot ssm trends by species
plot_state_space_abundance <- function(spp) {
  
  # read in JAGS model output data frame
  jags_output_all_species <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'state_space_results_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # plot trends by species
  jags_output_all_species %>%
    filter(species == spp) %>% # filter to specific species
    ggplot() +
    geom_ribbon(aes(x = year, ymin = lcl, ymax = ucl, group = species), alpha = 5/10, fill = 'grey40') +
    geom_line(aes(x = year, y = mean, group = species), size = 1) +
    geom_line(aes(x = year, y = n, group = species), linetype = 2, color = 'steelblue4') +
    geom_point(aes(x = year, y = n, group = species), color = 'steelblue4') +
    #geom_hline(aes(yintercept=), linetype="dashed")+
    theme_classic()+
    ylab("Estimated  breeding abundance") +
    xlab('Year') +
    # expand_limits(y = 0, x = c(1973, analysis_year))+
    # ggtitle("Modeled Breeding Mallard Abundance \n Wisconsin Aerial Spring Waterfowl Survey ") +
    scale_y_continuous(labels = scales::comma)
  
}


# plot map of aerial transects
plot_transect_map <- function() {
  
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
      transect < 30 ~ 'SEC', # SEC
      transect >= 30 & transect < 43 ~ 'NHI',
      transect >= 43 & transect < 61 ~ 'NLO',
      transect >= 61 & transect < 72 ~ 'SWD'
    )
  )
  
  # plot; this would be better if we could color transects by survey region...
  # should find and add 
  ggplot() +
    geom_sf(data = wi) +
    geom_sf(data = air_rts, aes(color = region), size = 1, show.legend = "line") +
    # geom_sf_text(data = air_rts, aes(label = transect), colour = "black") +
    geom_sf_text_repel(
      data = air_rts,
      aes(label = transect),
      size = 3,
      # additional parameters are passed to geom_text_repel()
      nudge_x = -.02,
      nudge_y = -.02,
      seed = 10 # same text position repeatable every year
    ) +
    theme_minimal() +
    scale_color_viridis_d(name = 'Region') +
    theme(axis.title = element_blank())
  
}


# plot crane counts (no groups vs total)
plot_crane_counts <- function() {
  
  # load data
  df_cranes <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'cranes', "_", analysis_year, '.csv'), show_col_types = FALSE)
  
  # plot
  df_cranes %>%
    rename(`Excluding groups` = cranes_no_groups, Total = cranes_total) %>%
    gather(count_type, count, -year) %>%
    ggplot() +
    geom_line(aes(year, count, color = count_type)) +
    geom_point(aes(year, count, color = count_type)) +
    scale_color_brewer(palette = 'Set1', direction = -1) +
    labs(
      y = 'Number of cranes',
      x = 'Year'
    ) +
    guides(color = guide_legend(title = "Count type")) +
    theme_classic() +
    theme(legend.position = 'top')
  
}


# plot air vs ground counts
plot_air_ground <- function(spp) {
  
  # load data
  df_air_ground <-  read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_air_vs_ground', '.csv'), show_col_types = FALSE)
  
  # filter to species
  df_air_ground <- df_air_ground %>%
    filter(p_species == spp & grd >= 1) %>%
    group_by(year, grd) %>%
    summarize(ind_birds_R = sum(ind_birds_R))

  # plot
  df_air_ground %>%
    mutate(
      grd = case_when(
        grd == 1 ~ 'Air',
        TRUE ~ 'Ground'
      )
    ) %>%
    ggplot() +
    geom_line(aes(year, ind_birds_R, color = as.factor(grd))) +
    scale_color_brewer(palette = 'Set1', name = 'Count type') +
    labs(
      y = 'Indicated breeding pairs',
      x = 'Year'
    ) +
    guides(color = guide_legend(title = "Count type")) +
    theme_classic() +
    theme(legend.position = 'top')
  
}


# other rmarkdown functions -----------------------------------------------

# extract mean, credible intervals for inserting those values into the report
extract_state_space_statistics <- function(spp) {
  
  # read in current year JAGS model output
  jags_output_all_species <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'state_space_results_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # filter to last 2 years for a particular species
  df <- jags_output_all_species %>%
    filter(species == spp) %>%
    slice_tail(n = 2)
  
  # calculate percent difference from previous year
  deviation_previous_year <- df %>%
    mutate(across(2, ~(.x/lag(.x) - 1) * 100)) %>%
    slice(2) %>%
    mutate(across(2, ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(across(2, ~str_c(.x, '%'))) %>%
    pull(mean)
  
  # use this information to say whether counts are lower or higher this year
  if (str_detect(deviation_previous_year, '-') == TRUE) {
    
    direction_previous_year <- 'lower'
    
  } else {
    
    direction_previous_year <- 'higher'
    
  }
  
  deviation_previous_year <- str_remove(deviation_previous_year, '-')
  
  # now do something similar for this year compared to long-term mean
  long_term_mean <- jags_output_all_species %>%
    filter(species == spp) %>%
    summarise(mean = mean(mean)) %>%
    bind_rows(., df %>% slice_tail(n = 1))
  
  deviation_long_term_mean <- long_term_mean %>%
    mutate(across(1, ~(.x/lag(.x) - 1) * 100)) %>%
    slice(2) %>%
    mutate(across(1, ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(across(1, ~str_c(.x, '%'))) %>%
    pull(mean)
  
  if (str_detect(deviation_long_term_mean, '-') == TRUE) {
    
    direction_long_term_mean <- 'lower'
    
  } else {
    
    direction_long_term_mean <- 'higher'
    
  }
  
  deviation_long_term_mean <- str_remove(deviation_long_term_mean, '-')
  
  # extract ssm trend statistics
  mean <- scales::number(df[2,2] %>% pull(), big.mark = ",", accuracy = 1) # current year mean
  lower <- scales::number(df[2,4] %>% pull(), big.mark = ",", accuracy = 1) # lower credible interval
  upper <- scales::number(df[2,5] %>% pull(), big.mark = ",", accuracy = 1) # upper credible interval
  previous_year_mean <- scales::number(df[1,2] %>% pull(), big.mark = ",", accuracy = 1) # previous year mean (needed for the abstract of final report)
  
  # bundle results together
  results <- tibble(
    mean = mean, 
    lower = lower, 
    upper = upper, 
    deviation_previous_year = deviation_previous_year, 
    direction_previous_year = direction_previous_year, 
    deviation_long_term_mean = deviation_long_term_mean, 
    direction_long_term_mean = direction_long_term_mean,
    previous_year_mean = previous_year_mean
    )
  return(results)
  
}


# extract wetland summaries for inserting those values into the report
extract_wetland_statistics <- function(region_code) {
  
  # lay out region codes here; need names for table captions
  regions <- tribble(
    ~region, ~name,
    1, 'SEC',
    2, 'NHI',
    3, 'NLO',
    4, 'SWD'
  )
  
  # filter out region name by region number
  region_name <- regions %>%
    filter(region == region_code) %>% 
    pull(name)
  
  # read in wetland summaries for creating table
  df_wetlands <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summaries_', analysis_year, '.csv'), show_col_types = FALSE)
  
  # figure out first year or surveys to display in table
  summary_start_year <- df_wetlands %>%
    ungroup() %>%
    select(year) %>%
    distinct() %>%
    slice_tail(n = 10) %>%
    slice(1) %>%
    pull(year)
  
  # grab last 10 years of surveys from current year
  # note: surveys not done in 2020 due to covid
  annual_wetlands <- df_wetlands %>%
    filter(region == region_code & year >= summary_start_year) %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region) %>%
    mutate(across(`I, II, VI`:Linear, ~format(round(.x, 1), nsmall = 1))) %>%
    ungroup() %>%
    mutate_all(as.character)
  
  # calculate % change from last survey year
  perc_change_last_year <- df_wetlands %>%
    filter(region == region_code & year >= summary_start_year) %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region) %>%
    ungroup() %>%
    slice_tail(n = 2) %>%
    mutate(across(`I, II, VI`:Linear, ~(.x/lag(.x) - 1) * 100)) %>%
    slice(2) %>%
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = '% Change from previous year') %>%
    mutate(across(`I, II, VI`:Linear, ~str_c(.x, '%')))
  
  # calculate 10-year mean
  ten_year_mean <- df_wetlands %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>%
    ungroup() %>%
    slice_tail(n = 10) %>% # grab most recent 10-year period
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    select(-region)
  
  # extract first, last year for labeling 10-year date span
  min_year <- min(ten_year_mean$Year)
  max_year <- max(ten_year_mean$Year)
  
  # format 10-year mean part of the table
  ten_year_mean <- ten_year_mean %>%
    summarise(across(`I, II, VI`:Linear, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = str_glue('10-Year mean ({min_year}-{max_year})')) %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear) %>%
    mutate(across(everything(), as.character))
  
  # calculate long-term mean (across every year of data)
  long_term_mean <- df_wetlands %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    ungroup() %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>%
    summarise(across(`I, II, VI`:Linear, ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(everything(), ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = 'Long-term mean') %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear) %>%
    mutate(across(everything(), as.character))
  
  # calculate current year
  current_year_wetlands <- df_wetlands %>%
    filter(region == region_code & year == analysis_year) %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    filter(region == region) %>%
    ungroup() %>%
    select(-region) %>%
    mutate(Year = as.character(Year))
  
  # calculate long-term mean again to calculate % change from long-term mean
  # should this not include the current year?
  long_term <- df_wetlands %>%
    rename(`I, II, VI` = i_ii_vi, `III` = iii, `IV, V` = iv_v, `VII, VIII` = vii_viii,
           Year = year, `Non-linear` = non_linear, Stream = stream, Ditch = ditch, Linear = linear) %>%
    ungroup() %>%
    drop_na() %>% # get rid of any no-survey years like 2020
    filter(region == region_code) %>%
    summarise(across(`I, II, VI`:Linear, ~mean(.x, na.rm = TRUE))) %>%
    mutate(Year = 'Long-term mean') %>%
    select(Year, `I, II, VI`, III, `IV, V`, `VII, VIII`, `Non-linear`, Stream, Ditch, Linear)
  
  # % change from long-term mean
  perc_long_term <- long_term %>%
    bind_rows(., current_year_wetlands) %>%
    mutate(across(`I, II, VI`:Linear, ~(.x/lag(.x) - 1) * 100)) %>%
    slice(2) %>%
    mutate(across(`I, II, VI`:Linear, ~format(round(.x, 1), nsmall = 1))) %>%
    mutate(Year = '% Change from long-term mean') %>%
    mutate(across(`I, II, VI`:Linear, ~str_c(.x, '%')))
  
  # combine dataframes needed for table
  wetland_summary <- perc_change_last_year %>%
    bind_rows(., long_term_mean) %>%
    bind_rows(., perc_long_term) %>%
    bind_rows(., ten_year_mean)
  
  # need this for reporting in report text
  wetland_summary <- wetland_summary %>%
    filter(Year %in% c('% Change from previous year', '% Change from long-term mean')) %>%
    select(Year, `Non-linear`, Linear) %>%
    janitor::clean_names() %>%
    gather(wetland_type, percent_change, -year) %>%
    mutate(year = str_extract(year, 'previous|long-term')) %>%
    mutate(
      direction = case_when(
        str_detect(percent_change, '-') == TRUE ~ 'down',
        TRUE ~ 'up'
      ),
      percent_change = str_remove(percent_change, '-')
    ) %>%
    pivot_wider(
      names_from = c(year, wetland_type),
      values_from = c(percent_change, direction),
    ) %>%
    janitor::clean_names()
  
  wetland_summary %>%
    write_csv(str_c(here::here('output'), '/', analysis_year, '/', 'wetland_summary_region_', region_code, '.csv'))
  
}

# percentage of mallards by region
mall_percentage <- function(){
  
  highest_region <- read_csv(str_c(here::here('output'), '/', analysis_year, '/', 'df_summary', '.csv'), show_col_types = FALSE) %>%
    select(species, region, area, b, r, pop_est, pop_se_2) %>%
    mutate(
      region = case_when(
        region == 1 ~ 'SEC',
        region == 2 ~ 'NHI',
        region == 3 ~ 'NLO',
        region == 4 ~ 'SWD'
      ),
      species = case_when(
        species == 1 ~ 'Mallard',
        species == 2 ~ 'Blue-winged teal',
        species == 3 ~ 'Wood duck',
        species == 4 ~ 'Canada goose',
        TRUE ~ 'Other duck species'
      )
    ) %>%
    filter(species == 'Mallard') %>%
    mutate(freq = (pop_est / sum(pop_est)) * 100) %>%
    arrange(freq) %>%
    slice(n = 4) # select highest region
  
  # report these in text
  tibble(
    region = highest_region$region,
    percent = round(highest_region$freq, 0)
  )
  
}
