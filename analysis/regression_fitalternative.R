args <- commandArgs(trailingOnly = TRUE)

library(tidyverse)
library(broom)
library(here)
library(vroom)
library(lme4)
library(cli)
library(glue)

source(here('analysis', 'utils_regression.R'))

# initialize ----

# select offense 
if(length(args)==0){
  offense_sel <- '11A'
  n_datasets <- 1
} else{
  offense_sel <- args[1]
  n_datasets <- args[2]
}


cli_text(glue("Offense selected: {offense_sel}"))
cli_text(glue("Number of datasets: {n_datasets}"))

# load file
files <- 1:n_datasets %>%
  paste0('data_imputed_', offense_sel, '_', ., '.csv')
df_c <- files %>% purrr::map( ~ vroom(here('data', .), col_types = cols()) %>%
                                create_design_matrix(., ori = TRUE))


# fit GLMM with random intercepts for ORIs ----

formula_X <- paste(c('(1 | originating_agency_identifier)', 
                      colnames(df_c[[1]] %>% select(-y, -originating_agency_identifier))), 
                    collapse = " + ")
cli_text(glue('Formula: {formula_X}'))

glmm_fit_all <- df_c %>% 
  map(~ glmer(
    paste('y ~ ', formula_X)
    ,data = . 
    ,family = binomial(link = "logit")
  ))

glmm_fit_all %>%
  map_dfr(~ summary(.) %>%
        coef() %>%
        data.frame() %>%
        rownames_to_column(., 'term') %>%
        filter(grepl('white', term))) %>%
  group_by(term) %>%
  summarise(estimate = mean(Estimate))

  
  