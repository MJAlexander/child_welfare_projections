prepare_stan_data <- function(df, covariates, this_race, meta_data, outcome_col, se_col){
  
  # extract meta data
  vars <- names(meta_data)
  for(i in 1:length(vars)){
    assign(vars[i], meta_data[[i]])
  }
  
  dfr <- df %>% filter(year %in% years, race==this_race) %>% 
    rename(temp = !!outcome_col,
           temp_se = !!se_col) %>% 
    mutate(temp = ifelse(is.infinite(temp), NA, temp),
           temp_se = ifelse(is.nan(temp_se), NA, temp_se)) %>% 
    mutate(temp = ifelse(is.na(temp),mean(temp, na.rm = TRUE), temp),
           temp_se = ifelse(is.na(temp_se),mean(temp_se, na.rm = TRUE), temp_se))
  
  # outcome
  y <- dfr %>% 
    select(temp) %>% 
    pull()
  
  y[is.na(y)] <- 0
  y[is.infinite(y)] <- 0
  
  # standard error of outcome
  se_y <- dfr %>% 
    select(temp_se) %>% 
    pull()
  
  
  ## indexes
  
  state_i <- dfr %>% 
    select(state) %>% 
    pull()
  
  state_i <- as.numeric(factor(state_i, levels = states))
  
  year_i <- dfr %>% 
    select(year) %>% 
    pull()
  
  year_i <- as.numeric(factor(year_i, levels = years))
  
  ## covariates
  
  covars = unique(covariates$variable)
  K <- length(covars)
  
  # mean-center
  X_c <- NULL
  for(i in 1:K){
    x <- dfr %>% 
      rename(this_x = eval(covars[i])) %>%
      ungroup() %>% 
      mutate(this_x = ifelse(is.na(this_x), mean(this_x, na.rm = T), this_x), # inpute NAs
             this_x = ifelse(this_x==0, 1, this_x)) %>% 
      ungroup() %>% 
      select(this_x) %>% 
      pull()
    if(str_detect(covars[i], "pct")){
      x <- log(x)
    }
    else{
      x <- log(x)
    }
    x_c <- x - mean(x)
    X_c <- cbind(X_c, x_c)
  }
  
  # region
  
  region_names <- df %>% 
    dplyr::select(state, division) %>% 
    group_by(state) %>% 
    slice(1) %>% 
    dplyr::select(division) %>% pull()
  
  region_s <- rep(NA, length(states))
  for(i in 1:length(states)){
    region_s[i] <- which(regions == region_names[i])
  }
  
  region_i <- dfr %>% 
    select(division) %>% 
    pull()
  
  region_i <- as.numeric(factor(region_i, levels = regions))
  
  ## create some variables for the projection
  
  proj_combi <- tibble(state = NA %>% as.character(), 
                       year = NA) %>% 
    complete(state = states, year = years[length(years)]+(1:P)) %>% 
    drop_na()
  
  state_j <- proj_combi %>% 
    select(state) %>% 
    pull()
  
  state_j <- as.numeric(factor(state_j, levels = states))
  region_j <- proj_combi %>% 
    select(state) %>% 
    left_join(tibble(state = states, division = region_names)) %>% 
    select(division) %>% 
    pull()
  
  region_j <- as.numeric(factor(region_j, levels = regions))
  
  year_j <- proj_combi %>% 
    select(year) %>% 
    pull()
  
  year_j <- as.numeric(factor(year_j, levels = years[length(years)]+(1:P)))
  
  
  # These are for projection of covariates
  
  last_obs <- X_c[year_i==length(years), ]
  second_last_obs <- X_c[year_i==(length(years)-1), ]
  third_last_obs <- X_c[year_i==(length(years)-2), ]
  
  
  # Input data into the model
  
  stan_data <- list(y = y,
                    se_y = se_y,
                    X = X_c,
                    year_c = years_c[1:length(years)],
                    P = P,
                    K = K,
                    R = length(regions),
                    region_s = region_s,
                    N = length(y), 
                    Y = length(years),
                    S = length(states),
                    M = length(state_j),
                    state_i = state_i, year_i = year_i, region_i = region_i,
                    state_j = state_j, year_j = year_j, region_j = region_j,
                    last_obs = last_obs,
                    second_last_obs = second_last_obs, 
                    third_last_obs = third_last_obs,
                    sigma_beta = 0.1,
                    sigma_eps = 0.05)
  
  return(list(stan_data = stan_data, dfr = dfr))
}