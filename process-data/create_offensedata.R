
suppressMessages(library(tidyverse))
suppressMessages(library(vroom))
suppressMessages(library(here))
suppressMessages(library(cli))


# define functions ----

write_df_for_offense <- function(list_df, offense_sel) {
  
  cli_text('Offense selected: ', offense_sel, '\n')
  
  df <- list_df$offense %>%
    filter(grepl(offense_sel, ucr_offense_code)) %>%
    inner_join(list_df$admin,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    inner_join(list_df$offender,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    inner_join(list_df$victim,
               by = c("originating_agency_identifier", "incident_number", "year")
    ) %>%
    ungroup()
  
  # check types of offenses included
  cli_text(paste0(unique(df$ucr_offense_code), collapse = '-'))
  
  offense_name <- paste0(
    "data_",
    case_when(
      offense_sel ==  "Murder/Nonnegligent Manslaughter|09A" ~ '09A',
      offense_sel == "Aggravated Assault|13A" ~ '13A',
      offense_sel == "Simple Assault|13B" ~ '13B',
      offense_sel == "Robbery|120" ~ '120',
      offense_sel == "Forcible Rape|11A" ~ '11A'
    ),
    ".csv"
  )
  
  df %>%
    select(-ucr_offense_code) %>%
    vroom_write(here("data", offense_name))
}


# filter incidents of interest ----

# get data of interest
list_df <- list(
  admin_batch = "admin_batch.csv",
  offender = "offender.csv",
  # arrestee = 'Arrestee_violent_1316.csv',
  offense = "offense.csv",
  victim = "victim.csv"
) %>%
  purrr::map(~ vroom(here("data", .x),
    col_types = cols()
  ))

# states submitting all their data through nibrs in 2014
states <- "AR|CO|DE|ID|IA|KY|MI|MT|NH|ND|SC|SD|TN|VT|VA|WV"

# subset incidents of interest
which_incidents <- list_df$admin_batch %>%
  # only in the states selected
  filter(grepl(states, state_abbv)) %>%
  # one offender
  filter(total_offender_segments == 1) %>%
  # one victim
  filter(total_victim_segments == 1)
which_incidents <- which_incidents %>%
  # victim is either white or black
  inner_join(list_df$victim %>% filter(grepl("White|Black", race_of_victim)) %>%
    distinct(originating_agency_identifier, incident_number, year), # unnecessary
  by = c("originating_agency_identifier", "incident_number", "year")
  )
which_incidents <- which_incidents %>%
  # offender is white or black
  inner_join(list_df$offender %>% filter(grepl("White|Black", race_of_offender))
    %>% distinct(originating_agency_identifier, incident_number, year), # unnecessary
  by = c("originating_agency_identifier", "incident_number", "year")
  ) %>%
  distinct(originating_agency_identifier, incident_number, year)

which_incidents %>% count(year)

# restrict data in the list to avoid join that is too cumbersome
list_df <- list_df %>% map(~ .x %>% inner_join(which_incidents,
  by = c("originating_agency_identifier", "incident_number", "year")
))

# keep only useful information in admin file
list_df$admin_batch <- list_df$admin_batch %>%
  mutate(y = ifelse(total_arrestee_segments>0,1,0)) %>%
  select(originating_agency_identifier, incident_number, 
         total_offense_segments, core_city, country_division,
         state_abbv, year, time_day,
         cleared_exceptionally, 
         time_day, city_name, y, 
         population_group, officer_rate,
         fips_county, msa_code, population_leoka, total_empl, total_off) %>%
  # next line is just to guarantee absence of duplicates, but it's unnecessary
  distinct(originating_agency_identifier, incident_number, year, .keep_all = T)

# attempt to fix NAs in counties with LEAIC files
leaic <- vroom(here('data', 'leaic.csv')) %>%
  rename(fips = FIPS) %>%
  distinct(originating_agency_identifier, .keep_all = TRUE)
list_df$admin_batch <- list_df$admin_batch %>%
  select(-fips_county) %>%
  left_join(leaic, by = 'originating_agency_identifier')

# rename offenses
list_df$offense <- list_df$offense %>%
  mutate(ucr_offense_code = case_when(
    ucr_offense_code == '(09A) Murder/nonnegligent manslaughter' ~ 'Murder/Nonnegligent Manslaughter',
    ucr_offense_code == '(11A) Forcible rape' | ucr_offense_code == '(11A) Rape' ~ 'Forcible Rape',
    ucr_offense_code == '(120) Robbery' ~ 'Robbery',
    ucr_offense_code == '(13A) Aggravated assault' ~ 'Aggravated Assault',
    ucr_offense_code == '(13B) Simple assault' ~ 'Simple Assault',
    TRUE ~ ucr_offense_code
  ))

# fix age of victim
list_df$victim <- list_df$victim %>%
  mutate(age_of_victim = as.numeric(ifelse(grepl('(00) Unknown', age_of_victim), NA,
                                           ifelse(grepl('NB|NN|BB', age_of_victim), NA, 
                                                  age_of_victim))))

list_df$offender <- list_df$offender %>%
  group_by(originating_agency_identifier) %>%
  mutate(frac_black_off = sum(race_of_offender=='Black')/n()) %>%
    ungroup

# write data ----

list(
  homicide = "Murder/Nonnegligent Manslaughter|09A",
  aggravated_assault = "Aggravated Assault|13A",
  simple_assault = "Simple Assault|13B",
  robbery = "Robbery|120",
  rape = "Forcible Rape|11A"
) %>%
  purrr::map(~ write_df_for_offense(
    list_df = list_df,
    offense_sel = .
  ))




