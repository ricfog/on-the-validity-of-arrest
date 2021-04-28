
suppressMessages(library(asciiSetupReader))
suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(vroom))
suppressMessages(library(cli))

# define functions ----

# read individual files
read_file <- function(txt) {
  df <- read_ascii_setup(
    data = txt,
    setup_file = str_replace(txt, "-Data.txt", "-Setup.sas")
  ) %>% tibble()
  
  colnames(df) <- tolower(colnames(df))

  # correct different names for columns over years
  if ("originating_agency_indentifier" %in% colnames(df)) {
    colnames(df)[which(colnames(df) == "originating_agency_indentifier")] <-
      "originating_agency_identifier"
  }
  if ("arrest_transaction_incident" %in% colnames(df)) {
    colnames(df)[which(colnames(df) == "arrest_transaction_incident")] <- 
      "incident_number"
  }

  return(df)
}

# clean all data for specific year
process_nibrs_data <- function(dir_sel) {

  cli_h1(dir_sel)
  
  # get the selected year_sel
  year_sel <- nibrs_items %>%
    filter(item == parse_number(dir_sel)) %>%
    pull(year_sel)
  files <- list.files(
    path = here("downloads", dir_sel),
    pattern = "\\.txt$",
    recursive = TRUE
  )
  files <- files[grepl("Data", files)]
  files <- files %>%
    map_chr(~ here("downloads", dir_sel, .x))

  # batch ----

  cli_text('batch')
  
  if (year_sel >= 2013) {
    batch_name <- files[1]
    batch <- read_file(txt = batch_name) %>%
      rename(
        fips_county = fips_county_1, state_abbv = state_abbreviation,
        msa_code = msa_code_1, population = last_population_1
      )
  } else {
    batch_name <- files[1:3]
    batch1 <- read_file(txt = batch_name[3]) %>%
      distinct(originating_agency_identifier, .keep_all = TRUE)
    batch2 <- read_file(txt = batch_name[2]) %>%
      distinct(
        originating_agency_identifier, current_population_1,
        ucr_county_code_1, msa_code_1
      )
    batch3 <- read_file(txt = batch_name[1]) %>%
      distinct(
        originating_agency_identifier, city_name, 
        state_abbreviation,
        core_city, country_division, country_region,
        covered_by_ori, population_group
      ) %>%
      rename(state_abbv = state_abbreviation)
    batch <- batch1 %>%
      inner_join(batch2, by = 'originating_agency_identifier') %>%
      inner_join(batch3, by = 'originating_agency_identifier') %>%
      rename(population = current_population_1, 
             fips_county = fips_county_1, 
             msa_code = msa_code_1) %>%
      distinct(
        state_abbv, originating_agency_identifier, city_name, population_group,
        country_division, country_region,
        population, core_city, covered_by_ori, msa_code, fips_county
      )
  }

  batch <- batch %>%
    mutate(year_sel = year_sel) %>%
    mutate(population_group = case_when(
      year_sel <= 2011 ~ population_group,
      year_sel >= 2012 ~ str_sub(population_group, start = 5)
    )) %>%
    mutate(
      population_group = trimws(population_group)
    ) %>%
    mutate(core_city = tolower(core_city)) %>%
    mutate(core_city = case_when(
      grepl('y', core_city) ~ 'Yes',
      grepl('n', core_city) ~ 'No',
      TRUE ~ core_city
    )) %>%
    mutate(country_division = case_when(
      year_sel > 2011 ~ tolower(str_sub(country_division, start = 5)),
      TRUE ~ tolower(country_division)
    ))

  batch <- batch %>%
    distinct(
      state_abbv, originating_agency_identifier,
      city_name, population_group, country_division, country_region,
      population,
      core_city, covered_by_ori, msa_code, fips_county
    )

  batch %>% vroom_write(here("data", "nibrs", paste0("batch_", year_sel, ".csv")))


  # offender ----

  cli_text('offender')
  
  offender <- read_file(files[ifelse(year_sel < 2013, 8, 6)])

  offender <- offender %>%
    mutate(race_of_offender = tolower(race_of_offender))

  if (!("ethnicity_of_offender" %in% colnames(offender))) {
    offender <- offender %>%
      mutate(ethnicity_of_offender = NA)
  } 


    offender <- offender %>%
      mutate(
        race_of_offender = case_when(
          grepl("black", race_of_offender) ~ "Black",
          grepl("white", race_of_offender) ~ "White",
          grepl("asian|isl", race_of_offender) ~ "Asian/Pacific islander",
          grepl("ind", race_of_offender) ~ "American Indian/Alaska Native"
        ),
        ethnicity_of_offender = tolower(ethnicity_of_offender),
        ethnicity_of_offender = case_when(
          ethnicity_of_offender == 'hispanic origin' | ethnicity_of_offender == "(h) hispanic or latino origin" ~ 'hispanic or latino',
          ethnicity_of_offender == 'not of hispanic origin' | ethnicity_of_offender == "(n) not hispanic or latino origin" ~ 'not hispanic or latino'
        )
      )


  offender <- offender %>%
    mutate(
      sex_of_offender = ifelse(grepl("F", sex_of_offender), "Female",
        ifelse(grepl("M", sex_of_offender), "Male", NA)
      ),
      age_of_offender = as.numeric(age_of_offender)
    ) #%>%
    #select(-segment_level, -numeric_state_code)

  offender %>% vroom_write(here("data", "nibrs", paste0("offender_", year_sel, ".csv")))


  # arrestee ----

  cli_text('arrestee')
  
  arrestee <- read_file(files[ifelse(year_sel < 2013, 9, 7)])
  
  if (!("ethnicity_of_arrestee" %in% colnames(arrestee))) {
    arrestee <- arrestee %>%
      mutate(ethnicity_of_arrestee = NA)
  } 
  

    arrestee <- arrestee %>%
    mutate(race_of_arrestee = tolower(race_of_arrestee),
        race_of_arrestee = case_when(
          grepl("black", race_of_arrestee) ~ "Black",
          grepl("white", race_of_arrestee) ~ "White",
          grepl("asian|isl", race_of_arrestee) ~ "Asian/Pacific islander",
          grepl("ind", race_of_arrestee) ~ "American Indian/Alaska Native"
        ),
        ethnicity_of_arrestee = tolower(ethnicity_of_arrestee),
        ethnicity_of_arrestee = case_when(
          ethnicity_of_arrestee == 'hispanic origin' | ethnicity_of_arrestee == "(h) hispanic or latino origin" ~ 'hispanic or latino',
          ethnicity_of_arrestee == 'not of hispanic origin' | ethnicity_of_arrestee == "(n) not hispanic or latino origin" ~ 'not hispanic or latino'
        )
      )


    arrestee <- arrestee %>%
      mutate(
        type_of_arrest = case_when(
          grepl("O", type_of_arrest) ~ "On-view arrest",
          grepl("S", type_of_arrest) ~ "Summoned/Cited",
          grepl("T", type_of_arrest) ~ "Taken into custody",
          TRUE ~ "Other"
        ),
        sex_of_arrestee = 
          case_when(
            grepl("F", sex_of_arrestee) ~ "Female",
            grepl("M", sex_of_arrestee) ~ "Male"
          ),
        age_of_arrestee = as.numeric(age_of_arrestee),
        resident_status_of_arrestee = case_when(
          grepl("R", resident_status_of_arrestee) ~ "Resident",
          grepl("N", resident_status_of_arrestee) ~ "Nonresident"
        )
      ) %>%
      select(-arrestee_armed_with_1, -arrestee_armed_with_2)

  arrestee %>% vroom_write(here("data", "nibrs", paste0("arrestee_", year_sel, ".csv")))

  # victim ----

  cli_text('victim')
  
  ## victim
  victim <- read_file(files[ifelse(year_sel < 2013, 7, 5)])

  colnames(victim)[grepl("assignment_type_", colnames(victim))] <- "assignment_type_officer"
  colnames(victim)[grepl("type_of_activity_officer", colnames(victim))] <- "type_of_activity_(officer)"
  colnames(victim)[grepl("homicide_circumstance_", colnames(victim))] <- paste0("agg_aslt_homicide_circumstance_", 1:2)

  victim <- victim %>%
    mutate(race_of_victim = tolower(race_of_victim))

  if (!("ethnicity_of_victim" %in% colnames(victim))) {
    victim <- victim %>%
      mutate(ethnicity_of_victim = NA)
  } 
  

    victim <- victim %>%
      mutate(
        race_of_victim = tolower(race_of_victim),
        race_of_victim = case_when(
          grepl("black", race_of_victim) ~ "Black",
          grepl("white", race_of_victim) ~ "White",
          grepl("asian|isl", race_of_victim) ~ "Asian/Pacific islander",
          grepl("ind", race_of_victim) ~ "American Indian/Alaska Native"
        ),
        ethnicity_of_victim = tolower(ethnicity_of_victim),
        ethnicity_of_victim = case_when(
          ethnicity_of_victim == 'hispanic origin' | ethnicity_of_victim == "(h) hispanic or latino origin" ~ 'hispanic or latino',
          ethnicity_of_victim == 'not of hispanic origin' | ethnicity_of_victim == "(n) not hispanic or latino origin" ~ 'not hispanic or latino'
        )
      )


  victim <- victim %>%
    mutate(
      sex_of_victim = ifelse(grepl("F", sex_of_victim), "Female", ifelse(grepl("M", sex_of_victim), "Male", NA)),
      age_of_victim = as.numeric(age_of_victim)
    ) %>%
    mutate(injury = case_when(
      year_sel > 2011 ~ tolower(str_sub(injury_1, start = 5)),
      TRUE ~ tolower(injury_1)
    )) %>%
    mutate(type_of_victim = case_when(
      year_sel > 2011 ~ tolower(str_sub(type_of_victim, start = 5)),
      TRUE ~ tolower(type_of_victim)
    )) %>%
    mutate(relationship_vic_to_off = case_when(
      year_sel > 2011 ~ tolower(str_sub(relationship_vic_to_off_1, start = 5)),
      TRUE ~ tolower(relationship_vic_to_off_1)
    ))
  
  victim %>%
    vroom_write(here("data", "nibrs", paste0("victim_", year_sel, ".csv")))


  # offense ----

  cli_text('offense')
  
  offense <- read_file(files[ifelse(year_sel < 2013, 5, 3)])

  ## rename columns in case you use the .rda files
  colnames(offense)[grepl("suspected_of_using_", colnames(offense))] <- paste0("offender_s_suspected_of_using_", 1:3)
  colnames(offense)[grepl("attempted", colnames(offense))] <- "offense_attempted_completed"
  colnames(offense)[grepl("weapon", colnames(offense))] <- paste0("weapon_force_", 1:3)

  offense <- offense %>%
    rename(weapon_force = weapon_force_1) %>%
    mutate(
      # weapon_force = fct_explicit_na(ifelse(`weapon_/_force_1` == '99', 'No weapon/force',
      #                                      'Weapon/force'), na_level = "(Missing)"),
      offense_attempted_completed = case_when(
        grepl("C", offense_attempted_completed) ~ "Completed",
        grepl("A", offense_attempted_completed) ~ "Attempted",
        TRUE ~ offense_attempted_completed
      ),
      use_alcohol = case_when(
        grepl("A", offender_s_suspected_of_using_1) | grepl("A", offender_s_suspected_of_using_2) | grepl("A", offender_s_suspected_of_using_3) ~ "Yes",
        TRUE ~ "No"
      ),
      use_drugs = case_when(
        grepl("D", offender_s_suspected_of_using_1) |
          grepl("D", offender_s_suspected_of_using_2) |
          grepl("D", offender_s_suspected_of_using_3) ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    mutate(weapon_force = case_when(
      year_sel > 2011 ~ tolower(str_sub(weapon_force, start = 6)),
      TRUE ~ tolower(weapon_force)
    )) %>%
    mutate(
      weapon_force = trimws(weapon_force)
    ) %>%
    mutate(location_type = case_when(
      year_sel > 2011 ~ tolower(str_sub(location_type, start = 6)),
      TRUE ~ tolower(location_type)
    ))

  offense %>%
    vroom_write(here("data", "nibrs", paste0("offense_", year_sel, ".csv")))


  # admin ----

  cli_text('admin')
  
  # admin file
  admin <- read_file(files[ifelse(year_sel < 2013, 4, 2)])

  admin <- admin %>%
    mutate(incident_hour = as.numeric(str_extract(incident_date_hour, "\\-*\\d+\\.*\\d*"))) %>%
    mutate(time_day = case_when(
      incident_hour <= 6 | incident_hour >= 21 ~ "At night",
      incident_hour > 6 & incident_hour < 21 ~ "During day"
    )) %>%
    mutate(cleared_exceptionally = case_when(
      year_sel > 2011 ~ tolower(str_sub(cleared_exceptionally, 5)),
      TRUE ~ tolower(cleared_exceptionally)
    )) %>% mutate(
      cleared_exceptionally = case_when(
        cleared_exceptionally == 'a' ~ 'death of offender',
        cleared_exceptionally == 'b' ~ 'prosecution declined',
        cleared_exceptionally == 'c' ~ 'in custody of other jurisdiction (includes extradition denied)',
        cleared_exceptionally == 'd' ~ 'victim refused to cooperate',
        cleared_exceptionally == 'e' ~ 'juvenile/no custody',
        cleared_exceptionally == 'n' ~ 'not applicable',
        cleared_exceptionally == 'extradition denied' ~ 'in custody of other jurisdiction (includes extradition denied)',
        cleared_exceptionally == 'prosecution declined (for other than lack of probable cause)' ~ 'prosecution declined',
        cleared_exceptionally == 'victim refused coop' ~ 'victim refused to cooperate',
        TRUE ~ cleared_exceptionally
      ))

  admin %>%
    #select(-segment_level) %>%
    vroom_write(here("data", "nibrs", paste0("admin_", year_sel, ".csv")))
}

# process files ----

nibrs_items <- tibble(
  item = c(
    25113, # 2007
    27647, # 2008
    30770, # 2009
    33530, # 2010
    34585, # 2011
    35035, # 2012
    36120, # 2013
    36398, # 2014
    36795, # 2015
    37065 # 2016
  ),
  year_sel = 2010:2016#2007:2016
)

if (!dir.exists(here("data", "nibrs"))) {
  dir.create(here("data", "nibrs"))
}

dirs <- list.files(path = here("downloads"))
dirs <- dirs[parse_number(dirs) %in% nibrs_items$item]

dirs %>%
  purrr::map(~ process_nibrs_data(.x))



