
suppressMessages(library(tidyverse))
suppressMessages(library(vroom))
suppressMessages(library(here))
suppressMessages(library(cli))

files <- list.files(here("data", "nibrs"))
years_sel <- parse_number(files) %>% unique()
cli_text(paste(years_sel, collapse = '-'))

# define crime types of interest
crime_types <- paste0(c(
  "13A", "13B", "09A", "120", "11A",
  "Murder/Nonnegligent Manslaughter", "Forcible Rape",
  "Robbery", "Simple Assault", "Aggravated Assault"
),
collapse = "|"
)

# keep only incidents where crime types match those of interest ----
which_incidents <- files[grepl("offense", files)] %>%
  purrr::map_dfr(~ mutate(vroom(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier,
      incident_number,
      ucr_offense_code
    )
  ), year = parse_number(.)) %>%
    filter(grepl(crime_types, ucr_offense_code)) %>%
    distinct(originating_agency_identifier, incident_number, year))
which_incidents %>% count(year)

# write offender file ----
files[grepl("offender", files)] %>%
  purrr::map(~ mutate(vroom(here("data", "nibrs", .),
    col_select = c(
      age_of_offender, sex_of_offender, race_of_offender, ethnicity_of_offender,
      originating_agency_identifier, incident_number
    ),
    col_types = cols(
      age_of_offender = col_integer(),
      ethnicity_of_offender = col_character()
    )
  ), year = parse_number(.)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  vroom_write(here("data", "offender.csv"))

# write offense file ----
files[grepl("offense", files)] %>%
  purrr::map(~ mutate(vroom(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier, incident_number, incident_date, ucr_offense_code,
      offense_attempted_completed, use_alcohol, use_drugs, location_type,
      weapon_force, weapon_force_2, weapon_force_3
    ),
    col_types = cols(weapon_force = col_character())
  ), year = parse_number(.)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  vroom_write(here("data", "offense.csv"))

# write victim file ----
files[grepl("victim", files)] %>%
  purrr::map(~ mutate(vroom(here("data", "nibrs", .),
    col_select = c(
      originating_agency_identifier, incident_number,
      age_of_victim, relationship_vic_to_off, injury,
      sex_of_victim, race_of_victim, ethnicity_of_victim,
      type_of_victim, `type_of_activity_(officer)` # ,
      # additional_justifiable_homicide_circumstances
    ),
    col_types = cols(age_of_victim = col_character())
  ),
  year = parse_number(.)
  ) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x)),
    by = c("originating_agency_identifier", "incident_number", "year")
    )) %>%
  bind_rows() %>%
  vroom_write(here("data", "victim.csv"))

# write admin file ----
files[grepl("admin", files)] %>%
  purrr::map(~ mutate(vroom(here("data", "nibrs", .),
    col_types = cols()
  ), year = parse_number(.)) %>%
    select(-segment_level, -incident_date_hour) %>%
    mutate(incident_hour = as.character(incident_hour)) %>%
    inner_join(which_incidents %>%
      filter(year == parse_number(.x))
    ) %>%
    inner_join(
      vroom(here("data", "nibrs", paste0("batch_", parse_number(.x), ".csv")),
        col_types = cols()
      )
    )) %>%
  bind_rows() %>%
  left_join(
    vroom(here("data", "leoka.csv")),
    by = c("originating_agency_identifier", "year")
  ) %>%
  vroom_write(here("data", "admin_batch.csv"))


