
suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(haven))
suppressMessages(library(vroom))

# define functions ----

clean_data <- function(df) {
    
  df <- df %>%
    rename(originating_agency_identifier = ORI9) %>%
    distinct(originating_agency_identifier, FIPS)
  
  return(df)
}


# clean files ----

leaic_items <- 35158

files <- list.files(path = here("downloads"), 
                    pattern = "\\.dta$", 
                    recursive = TRUE)
files <- files[parse_number(files) %in% leaic_items]
files <- files %>%
  map_chr(~ here("downloads", .x))

df <- files %>%
  map(~ clean_data(read_dta(.x))) %>%
  bind_rows()

df %>% vroom_write(here("data", "leaic.csv"))



