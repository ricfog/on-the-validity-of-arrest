
suppressMessages(library(vroom))
suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(mice))
suppressMessages(library(cli))
suppressMessages(library(glue))


# define functions ----

# design matrix with dummy features
get_dummy_features <- function(X){

  if('injury' %in% colnames(X)){
    X <- X %>%
      mutate(
        is_serious_injury = ifelse(injury== 'N' | injury == 'M' | grepl('none', tolower(injury)) | grepl('minor', tolower(injury)), 0, 1),
        is_minor_injury = ifelse(injury == 'M' | grepl('minor', injury), 0, 1)
      ) %>%
      select(-injury)
  }
  
  X <- X %>%
    mutate(
      is_offender_stranger = ifelse(grepl('stranger|unknown', tolower(relationship_vic_to_off)), 1, 0),
      is_residence = ifelse(grepl('20|residence', tolower(location_type)), 1, 0),
      is_firearm = case_when(grepl('11|12|13|14|15|firearm|handgun|rifle|shotgun', tolower(weapon_force)) | 
                                  grepl('11|12|13|14|15|firearm|handgun|rifle|shotgun', tolower(weapon_force_2)) | 
                                          grepl('11|12|13|14|15|firearm|handgun|rifle|shotgun', tolower(weapon_force_3)) ~ 1, 
                             tolower(weapon_force) != 'unknown' ~ 0),
      is_other_weapon = case_when(grepl('drugs|blunt|explosives|incendiary|knife|motor|poison', tolower(weapon_force)) | 
                                       grepl('drugs|explosives|incendiary|knife|motor|poison', tolower(weapon_force_2)) | 
                                               grepl('drugs|explosives|incendiary|knife|motor|poison', tolower(weapon_force_3)) ~ 1, 
                                  tolower(weapon_force) != 'unknown' ~ 0),
      is_multiple_offense = ifelse(total_offense_segments>1, 1, 0),
      use_alcohol = ifelse(use_alcohol == 'Yes', 1, 0),
      use_drugs = ifelse(use_drugs == 'Yes', 1, 0),
      is_core_city = case_when(core_city == "Yes" ~ 1, 
                               core_city == 'No' ~ 0),
      is_during_day = case_when(time_day == 'During day' ~ 1, 
                                time_day == 'At night' ~ 0),
      is_offender_male = case_when(sex_of_offender == 'Male' ~ 1, 
                                   sex_of_offender=='Female' ~ 0),
      is_victim_male = case_when(sex_of_victim == 'Male' ~ 1, 
                                 sex_of_victim == 'Female'~ 0),
      is_offender_white = ifelse(race_of_offender == 'White', 1, 0),
      is_victim_white = ifelse(race_of_victim == 'White', 1, 0),
      region = case_when(
        grepl('AR|DE|KY|SC|TN|VA|WV', state_abbv) ~ 'South',
        grepl('NH|VT', state_abbv) ~ 'Northeast',
        grepl('CO|ID|MT', state_abbv) ~ 'West',
        TRUE ~ 'Midwest'
      )
    )  %>%
    select(-relationship_vic_to_off, -location_type,
           -weapon_force, -weapon_force_2, -weapon_force_3,
           -time_day, -core_city,
           -sex_of_offender, -sex_of_victim,
           -race_of_offender, -race_of_victim, 
           -country_division
    ) %>%
    droplevels()
  
  if('offense_attempted_completed' %in% colnames(X)){
    X <- X %>%
      mutate(is_offense_attempted = ifelse(offense_attempted_completed=='Attempted', 1, 0)) %>%
      select(-offense_attempted_completed)
  }
  
  
  X %>%
    group_by(originating_agency_identifier) %>%
    rename(
      officer_per_1000capita = officer_rate,
      population_served = population_leoka
    ) %>%
    mutate(
      #offense_per_1000capita = n() / population_leoka * 1000,
      year = as.factor(year)
    ) %>% ungroup() %>%
    select(-matches('total_empl|leoka|total_off|officer_rate|region', everything()))
  
}


write_imputed_df <- function(df, offense_sel){

  cli_text(glue('Offense selected: {offense_sel}'))
  
  # drop incidents with exceptional clearance
  df <- df %>% filter(cleared_exceptionally == 'not applicable')

  y <- df$y
  X <- df %>% select(-y) %>% 
    select(-cleared_exceptionally, -city_name, 
           -msa_code, -ethnicity_of_offender, 
           -ethnicity_of_victim,
           -type_of_victim, 
           #-population_leoka,
           -`type_of_activity_(officer)`
           #-population_group, 
           ,-incident_number, -incident_date
           ) %>%
   mutate(fips = case_when(
        is.na(fips) ~ originating_agency_identifier,
        TRUE ~ fips))

  if(offense_sel == '09A') X <- X %>% 
    select(-any_of(matches('injury', everything())))

  X <- X %>% get_dummy_features(.)

  if(offense_sel == '13B') X <- X %>%
    select(-is_other_weapon, -is_firearm)
  
  if(offense_sel == '11A') X <- X %>%
    mutate(is_offender_male = case_when(
      is.na(is_offender_male) ~ 0,
      TRUE ~ is_offender_male
    ))
  
  var_onlyoneval <- c(colnames(df)[apply(df, 2, function(x) length(unique(x)))==1],
  # other variables to be dropped for the imputation
  'originating_agency_identifier', 'fips', 'country_division'
  )
  X_onlyoneval <- X %>% select(matches(var_onlyoneval))
  X <- X %>%
    select(-matches(var_onlyoneval)) %>% droplevels()

  glimpse(X)

  # select columns
  cli_h1('Missing values before imputation')
  nas <- colSums(is.na(X))/nrow(X)
  print(sort(round(nas[nas>0],3)) %>% rev())
  
  set.seed(13213432)

  # impute datasets
  cli_h1('Imputation')
  m_df <- 10
  X_imputed <- mice(X, maxit = 20, m = m_df, defaultmethod = c("pmm", "logreg", "polyreg", "polr"))
  
  #plot(X_imputed, y = which(nas>0) %>% names())

  list_X_imputed <- 1:m_df %>%
    map(~ complete(X_imputed, .))
  
  # transformed features
  colnames(list_X_imputed[[1]])
  # check absence of NAs
  cli_h1('Missing values after imputation in first dataset')
  print(colSums(is.na(X_imputed %>% complete(.,1))))
  
  # drop constants
  list_X_imputed <- list_X_imputed %>% purrr::map(~ Filter(function(x)(length(unique(x))>1), .x))
  
  list_X_imputed <- list_X_imputed %>% purrr::map( ~ .x %>% 
                                                     bind_cols(X_onlyoneval) %>%
                                                     add_column(y = y)
                                                     )
  
  w <- list_X_imputed %>%
    setNames(1:m_df) %>%
    purrr::imap(~ vroom_write(.x, 
                              here('data', glue('data_imputed_{offense_sel}_{.y}.csv'))))
}


# impute missing values ----

files <- list.files(path=here('data'), pattern="*.csv")
files <- files[grepl('data_', files) & !grepl('imputed', files)]


args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
} else {
  offense_input <- args[1]
  cli_text(glue("Offense type selected: {offense_input}"))
  files <- files[grepl(offense_input, files)]
}


# load files
list_df <- files %>%
  purrr::map(~vroom(here('data', .x),
  col_types = cols()))
names_df <- str_replace(word(files, 2, sep = "_"), '.csv', '')

# filter data of interest
map2(list_df,
     names_df,
     ~ write_imputed_df(.x, .y))



