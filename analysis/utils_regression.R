# create data for regression
create_design_matrix <- function(df, ori=FALSE, fips=FALSE){
  
  df <- df %>% 
    mutate(year = as.factor(year))
  
  if(ori){
    df <- df %>%
      select(-frac_black_off, -officer_per_1000capita, 
             -population_served, -state_abbv,
             -year, -is_core_city, -population_group) %>%
      select(-fips)
  } else if(fips){
    df <- df %>%
      select(-frac_black_off, -officer_per_1000capita, 
             -population_served, -state_abbv,
             -year, -is_core_city, -population_group) %>%
      select(-originating_agency_identifier)
  } else{
    df <- df %>%
      dplyr::select(-fips, -population_group)
  }
  
  df
}

# generate dummies for fastglm
format_data_fastglm <- function(df, formula_char){
  
  # reformat data for fastglm
  X <- model.matrix(as.formula(formula_char), df)[,-1]
  # get the response
  y <- df$y
  # create the matrix with dummy variables for regression
  df <- data.frame(X) %>% bind_cols(y = y)
  
  return(df)
}

# fit regression model with fastglm
fit_fastlogreg <- function(df){
  #cli_li('Fitting logistic regression...')
  
  df <- as.matrix(df)
  Intercept <- 1
  location_y <- which(colnames(df)=='y')
  mod_fit <- fastglm(x = cbind(Intercept, df[,-location_y]), 
                     y=as.numeric(as.vector(df[,location_y])), 
                     family = binomial())
  tibble(term = names(mod_fit$coefficients), estimate = mod_fit$coefficients)
}

# fit regression model with glm
fit_logreg <- function(formula, df){
  cli_li('Fitting logistic regression...')
  mod_fit <- glm(formula, data=df, family = binomial())
  mod_fit
}

# get covariance matrix of coefficients estimates from the sandwich package
get_cov <- function(mod_fit, sandcluster=NULL, boot=NULL){
  mod_fit_store <- tidy(mod_fit) %>%
    select(term, estimate, std.error)
  
  Vsand <- sandwich(mod_fit)
  std.sand <- tibble(std.error.sand = sqrt(diag(Vsand)),
                     term = names(diag(Vsand)))
  mod_fit_store <- mod_fit_store %>% 
    left_join(std.sand, by = 'term')
  
  if(!is.null(sandcluster)){
    Vsandcluster <- vcovCL(mod_fit, cluster = sandcluster)
    std.sand.cluster <- tibble(std.error.sandcl = sqrt(diag(Vsandcluster)),
                               term = names(diag(Vsandcluster)))
    mod_fit_store <- mod_fit_store %>% 
      left_join(std.sand.cluster, by = 'term')
  } 
  if(!is.null(boot)){
    Vboot <- vcovBS(mod_fit)
    std.boot <- tibble(std.boot = sqrt(diag(Vboot)),
                       term = names(diag(Vboot)))
    mod_fit_store <- mod_fit_store %>% 
      left_join(std.boot, by = 'term')
  }
  mod_fit_store
}

# focal slope: subset only 
get_df_rwgt <- function(df, bins_assigned, bin_chosen){
  Filter(function(x) (length(unique(x)) > 1), 
         df[bins_assigned$bin == bin_chosen, ] %>% slice_sample(n = nrow(.), replace = T))
}

# focal slope: compute values for teh reweighting 
get_grid_values <- function(x, method = "even", n_grid = 10, quant_grid = 0.1) {
  if (method == "even") {
    lb <- quantile(x, probs = 0.025)
    ub <- quantile(x, probs = 0.975)
    
    out <- seq(lb, ub, length = n_grid)
  } else if (method == "quantile") {
    out <- quantile(x, probs = seq(0.05, 0.95, by = quant_grid))
  }
  return(out)
}

# focal slope: main function
get_est_under_rwgt <- function(varname, df, B = 100) {
  cli_h1(glue('Variable: {varname}'))
  B_list <- as.list(1:B)
  names(B_list) <- 1:B
  
  # variable to be reweighted (dummy vs not)
  if (varname %in% c(
    "frac_black_off", "officer_per_1000capita",
    "age_of_offender",
    "population_served",
    "age_of_victim"
  )) {
    grid_values <- df %>%
      select(-y) %>%
      select_at(varname) %>%
      summarise_all(function(x) get_grid_values(x, method = "even"))
  } else {
    grid_values <- tibble(values = c(0, 1))
    colnames(grid_values) <- varname
  }
  
  centers <- pull(grid_values, !!varname) %>% unique()
  values <- pull(df, !!varname)
  
  # assign observation to the closest bin center
  bins_assigned <- tibble(
    n_obs = 1:nrow(df),
    bin = as.list(values) %>%
      purrr::map(~ which.min(abs(.x - centers))) %>%
      unlist()
  )
  
  cli_text('Bins assigned: {length(unique(bins_assigned$bin))}')
  
  # reweighting
  coefs <- tibble()
  for (bin_chosen in 1:length(centers)) {
    cli_text(glue('Bin chosen: {bin_chosen}, dimension: {nrow(df[bins_assigned$bin == bin_chosen, ])}'))
    if (length(unique(df[bins_assigned$bin == bin_chosen, ]$y)) > 1) {
      coefs <- coefs %>%
        bind_rows(tibble(
          center = centers[bin_chosen],
          boot_out = B_list %>% furrr::future_map(~ fit_fastlogreg(get_df_rwgt(df, bins_assigned, bin_chosen)),
                                                  .progress = TRUE,
                                                  seed = TRUE),
          b = 1:B
        ))
    }
  }
  
  coefs
}