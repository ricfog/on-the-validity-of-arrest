
options("icpsr_email" = "XYZ",
        "icpsr_password" = "XYZ")

suppressMessages(library(icpsrdata))
suppressMessages(library(tidyverse))
suppressMessages(library(here))

# download files ----

# study numbers for nibrs from icpsr
nibrs_items <- c(
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
)

nibrs_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))

# study numbers for leoka from icpsr
leoka_items <- c(
  25104, # 2007
  27646, # 2008
  30765, # 2009
  33525, # 2010
  34584, # 2011
  35020, # 2012
  36119, # 2013
  36395, # 2014
  36791, # 2015
  37062 # 2016
)

leoka_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))


# LEAIC
leaic_items <- 35158

leaic_items %>%
  walk(~ .x %>% icpsr_download(
    file_id = .,
    download_dir = here("downloads")
  ))


