
# associate color with division
get_color_division <- function(div){
  if(grepl('1', div)) x <- 'blue1'
  if(grepl('3', div)) x <- 'red'
  if(grepl('4', div)) x <- 'maroon'
  if(grepl('5', div)) x <- 'purple3'
  if(grepl('6', div)) x <- 'midnightblue'
  if(grepl('7', div)) x <- 'orange'
  if(grepl('8', div)) x <- 'black'
  return(x)
}

# knitr
sable <- function(x, escape = T) {
  kable(x = x, format = "html", digits = 2, 
               align = "c", escape = TRUE) %>% 
    kable_styling(position = "center")
}

# rename offenses
rename_offenses <- function(x){
  if(x == '09A') x <- 'Murder/non-negligent manslaughter'
  if(x == '11A') x <- 'Forcible rape'
  if(x == '120') x <- 'Robbery'
  if(x == '13A') x <- 'Aggravated assault'
  if(x == '13B') x <- 'Simple assault'
  x
}

# get list of states and country divisions
get_country_div_and_state <- function(){
  states_order <- c("NH", "VT", "MI", "IA", "ND", "SD", "DE", "SC", "VA",
                    "WV", "KY", "TN", "AR", "CO", "ID", "MT")
  country_div_order <- c("(1) New England", "(1) New England" , "(3) East North Central", 
                         "(4) West North Central", "(4) West North Central", 
                         "(4) West North Central",
                         "(5) South Atlantic", "(5) South Atlantic",
                         "(5) South Atlantic" ,"(5) South Atlantic", 
                         "(6) East South Central", 
                         "(6) East South Central", "(7) West South Central", 
                         "(8) Mountain","(8) Mountain","(8) Mountain") 
  return(list(states_order = states_order, country_div_order = country_div_order))
}


get_stars <- function(x){
  stats::symnum(x, corr = FALSE, na = FALSE,
                legend = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "** ", "*  ", ".  ", "   "))
}

est2char <- function(x, digits=2, std.error = FALSE){
  
  x <- round(x,2)
  x <- as.character(x)
  x_after_dot <- str_split(x, '\\.')[[1]][2]
  if(!(str_split(x, '')[[1]][1]=='-') & !std.error){
    x <- paste0('\\phantom{-}', x)
  }
  
  if(!grepl('\\.', x)){
    x_after_dot <- paste0(rep("0", digits), collapse = '')
    x <- paste0(x, '.', x_after_dot)
  } else if(nchar(x_after_dot)<digits){
    x <- paste0(x, rep(0, digits - nchar(x_after_dot)))
  } else{
    x
  }
  return(x)
}

to2char <- function(to, digits){
  to %>%
    rowwise() %>%
    mutate(estimate = est2char(estimate, 2, FALSE),
           std.error = est2char(std.error, 2, TRUE)) %>%
    mutate(sig = get_stars(p.value)) %>%
    mutate(estimate = glue('{estimate} ({std.error}){sig}')) %>%
    mutate(term = rename_vars(term)) %>%
    mutate(offense = rename_offenses(offense)) %>%
    ungroup %>%
    select(offense, term, estimate)
  
}

reorder_and_filter_rows <- function(coefs){
  coefs %>% filter(grepl('Intercept', term)) %>% arrange(term) %>%
    bind_rows(coefs %>% filter(grepl('Offender male|Offender white|Age of offender', term)) %>% 
                arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Victim male|Victim white|Age of victim', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('injury', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('stranger|Residence|day', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('substance|alcohol', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Firearm|weapon', term)) %>% arrange(term))  %>%
    bind_rows(coefs %>% filter(grepl('completed', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('ORI', term)) %>% arrange(term)) %>%
    bind_rows(coefs %>% filter(grepl('Core', term)) %>% arrange(term))
}


rename_vars <- function(vars){
  if(vars=='(Intercept)') vars <- 'Intercept'
  if(vars=='age_of_offender') vars <- 'Age of offender'
  if(vars=='age_of_victim') vars <- 'Age of victim'
  if(vars=='use_alcohol') vars <- 'Offender alcohol use'
  if(vars=='use_drugs') vars <- 'Offender substance use'
  if(vars=='is_multiple_offense') vars <- 'Multiple offenses' # check this
  if(vars=='is_offender_stranger') vars <- 'Offender stranger'
  if(vars=='is_serious_injury') vars <- 'Serious injury'
  if(vars=='is_minor_injury') vars <- 'Minor injury'
  if(vars=='is_residence') vars <- 'Residence'
  if(vars=='is_firearm') vars <- 'Firearm present'
  if(vars=='is_other_weapon') vars <- 'Other weapon present'
  if(vars=='is_victim_male') vars <- 'Victim male'
  if(vars=='is_offender_male') vars <- 'Offender male'
  if(vars=='is_vic_off_black_white') vars <- 'Vic Black-Off White'
  if(vars=='is_vic_off_white_black') vars <- 'Vic White-Off Black'
  if(vars=='is_vic_off_white_white') vars <- 'Vic White-Off White'
  if(vars=='is_victim_white') vars <- 'Victim white'
  if(vars=='is_offender_white') vars <- 'Offender white'
  if(vars=='is_core_city') vars <- 'Core city'
  if(vars=='is_gun') vars <- 'Firearm'
  if(vars=='other_weapon') vars <- 'Other weapon/force'
  if(vars=='is_during_day') vars <- 'During day'
  if(vars=='officer_rate') vars <- 'Officer rate (ORI)'
  if(vars=='population_served') vars <- 'Population served (ORI)'
  if(vars=='frac_black_off') vars <- '\\% black offenders (ORI)'
  if(vars=='offense_per_1000capita') vars <- '\\# Offenses per 1000 capita (ORI)'
  if(vars=='officer_per_1000capita') vars <- '\\# Officers per 1000 capita (ORI)'
  if(vars=='is_offense_attempted') vars <- 'Offense not completed'
  vars
}


