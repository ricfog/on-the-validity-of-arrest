args <- commandArgs(trailingOnly = TRUE)
options(future.globals.maxSize= 9999999999)

library(tidyverse)
library(broom)
library(here)
library(vroom)
library(future)
library(fastglm)
library(sandwich)
library(cli)
library(glue)

source(here('analysis', 'utils_regression.R'))

# initialize ----

# select offense (unless given from the command line)
if(length(args)==0){
  offense_sel <- '13A'
  n_cores <- 3
  skip_fs <- NULL
} else{
  offense_sel <- args[1]
  n_cores <- args[2]
  if(length(args)==3){skip_fs <- args[3]} else{skip_fs <- NULL}
}
n_datasets <- 10

cli_h1(glue("Offense selected: {offense_sel}"))
cli_text(glue("Number of cores: {n_cores}"))
cli_text(glue("Number of datasets: {n_datasets}"))

# initialize furrr
plan(multicore, workers = as.integer(n_cores))

# load files and set up models ----

# load files
files <- 1:n_datasets %>%
  paste0('data_imputed_', offense_sel, '_', ., '.csv')
df_c <- files %>% map( ~ vroom(here('data', .), col_types = cols()) %>%
                                create_design_matrix(.))

# formula for regression
formula_X <- paste0(colnames(df_c[[1]] %>% select(-y, -originating_agency_identifier)), 
                    collapse = "+")
formula_char <- paste('~', formula_X)
cli_text(glue('Formula: {formula_char}'))


# run logistic regression ----

cli_h2('Fit model on full data')

mod_fit_c <- df_c %>% map(~ format_data_fastglm(., formula_char) %>%
                            fit_logreg(formula='y~.', df=.))

# get model's output with sandwich standard errors
mod_fit_store <- 1:n_datasets %>% map_dfr(~ get_cov(mod_fit_c[[.x]]
                                                 ,sandcluster = pull(df_c[[.x]], originating_agency_identifier)
                                                 ), .id = 'n_dataset')

# store
w <- mod_fit_store %>%
  vroom_write(here('analysis', "regression-results",
                   glue("reg_{offense_sel}_original_logreg.csv")))


# focal slope model diagnostics ----

if(!is.null(skip_fs)){
  cli_text('Skipping model diagnostics!')
  quit()
}

cli_h2('Fit model under reweighting')

# model diagnostics - here we go only with the first dataset (nb 1)
df <- df_c[[1]] %>% select(-originating_agency_identifier)

df <- format_data_fastglm(df, formula_char)

terms_to_rwgt <- c( 
  'is_offender_male', 
  'is_victim_male', 'is_victim_white', 
  'age_of_offender', 'age_of_victim', 'is_serious_injury', 
  'is_offender_stranger', 'is_multiple_offense', 
  'is_serious_injury', 'is_minor_injury',
  'officer_per_1000capita', 'population_served', 'frac_black_off')
if(offense_sel == '09A') terms_to_rwgt <- setdiff(terms_to_rwgt, c('is_serious_injury', 'is_minor_injury'))
if(offense_sel == '11A') terms_to_rwgt <- setdiff(terms_to_rwgt, c('is_offender_male', 'is_victim_male'))
names(terms_to_rwgt) <- unlist(terms_to_rwgt)

est_under_rwgt <- terms_to_rwgt %>% 
  map(~ get_est_under_rwgt(.x[[1]], df, B=100)) %>%
  bind_rows(.id = 'term_rwgt')

# store focal slope
w <- est_under_rwgt %>% unnest(boot_out) %>% 
  vroom_write(here('analysis', "regression-results", 
                   glue("reg_{offense_sel}_reweighting_logreg.csv")))