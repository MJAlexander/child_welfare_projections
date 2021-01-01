write_save_model_results <- function(mod, stan_data, dfr, covariates, 
                                     this_race, meta_data, outcome_col, outcome_name){
  
  # extract meta data
  vars <- names(meta_data)
  for(i in 1:length(vars)){
    assign(vars[i], meta_data[[i]])
  }
  
  # extract stan data 
  vars <- names(stan_data)
  for(i in 1:length(vars)){
    assign(vars[i], stan_data[[i]])
  }
  
  # covars list
  covars = unique(covariates$variable)
  
  yhat <- mod %>% 
    gather_draws(mu[i]) %>% 
    mutate(fit = exp(.value)) %>% 
    median_qi(.width = c(0.5, 0.75,0.8, 0.95)) %>% 
    ungroup() %>% 
    mutate(state = states[state_i[i]], year = years[year_i[i]]) %>% 
    left_join(dfr %>% select(state, year, !!outcome_col))
  
  proj <-  mod %>% 
    gather_draws(mu_p[i]) %>% 
    mutate(fit = exp(.value)) %>% 
    median_qi(.width = c(0.5, 0.75, 0.8, 0.95)) %>% 
    ungroup() %>% 
    mutate(state = states[state_j[i]], year = years_proj[year_j[i]]) 
  
  res <- yhat %>% 
    select(state, year, fit:.width) %>% 
    bind_rows(proj %>% select(state, year, fit:.width)) %>% 
    arrange(state, year) %>% 
    left_join(dfr %>% select(state, year, !!outcome_col)) %>% 
    rename(ci_width = .width,
           fit_lower = fit.lower,
           fit_upper = fit.upper,
           observed = !!outcome_col) %>% 
    select(state, year, observed, fit:ci_width)
  
  # add in populations
  
  res <- res %>% 
    left_join(dfr %>% select(state, year, pop) %>% distinct()) %>% 
    group_by(state) %>% 
    mutate(pop = ifelse(year>years[length(years)], pop[year==years[length(years)]], pop)) %>% 
    mutate_at(.vars = vars(observed:fit_upper), .funs = funs(pop = .*pop))
  
  write_csv(res, paste0("results/", outcome_name,"_obs_fit_",this_race,".csv"))

  # Coefficients
  
  b0 <- mod %>% 
    gather_draws(beta0[condition]) %>% 
    median_qi() %>%  
    mutate(state = (states[condition])) %>% 
    ungroup() %>% 
    mutate(state = fct_reorder(factor(state), (.value))) %>% 
    select(state, .variable, .value:.width) %>% 
    rename(coefficient = .variable, value = .value, lower = .lower, upper = .upper, ci_width = .width) %>% 
    mutate(k = NA %>% as.integer(), covariate_name = "intercept") %>% 
    select(state, covariate_name, coefficient, value:ci_width)
  
  betas <- mod %>%
    gather_draws(beta[k,j, condition]) %>%
    median_qi(.width = c(0.25, 0.4, 0.5, 0.75, 0.8, 0.95)) %>%
    mutate(year = (years[condition]), division = regions[j]) %>%
    ungroup() %>%
    select(division, year, .variable,k, .value:.width) %>%
    rename(coefficient = .variable, value = .value, lower = .lower, upper = .upper, ci_width = .width) %>%
    mutate(covariate_name = covars[k])  %>%
    select(division, year, covariate_name, coefficient, k, value:ci_width)
  
  betas_proj <- mod %>%
    gather_draws(beta_p[k,j, condition]) %>%
    median_qi(.width = c(0.25, 0.4, 0.5, 0.75, 0.8, 0.95)) %>%
    mutate(year = (years_proj[condition]), division = regions[j]) %>%
    ungroup() %>%
    select(division, year, .variable,k, .value:.width) %>%
    rename(coefficient = .variable, value = .value, lower = .lower, upper = .upper, ci_width = .width) %>%
    mutate(covariate_name = covars[k])  %>%
    select(division, year, covariate_name, coefficient, k, value:ci_width)
  
  betas <- betas %>%
    bind_rows(betas_proj) %>%
    left_join(covariates %>% select(variable, variable_description, category) %>% rename(covariate_name = variable))
  
  write_csv(b0, paste0("results/b0_",outcome_name, "_",this_race,".csv"))
  write_csv(betas, paste0("results/betas_", outcome_name, "_", this_race,".csv"))
  
  # pr increase
  
  res_pr_increase <- bind_rows(mod %>% 
                                 gather_draws(mu[i]) %>% 
                                 mutate(fit = exp(.value)) %>% 
                                 mutate(state = states[state_i[i]], year = years[year_i[i]]),
                               mod %>% 
                                 gather_draws(mu_p[i]) %>% 
                                 mutate(fit = exp(.value)) %>% 
                                 mutate(state = states[state_j[i]], year = years_proj[year_j[i]])
  ) %>% 
    group_by(state, .draw) %>% 
    mutate(gone_up = fit>lag(fit),
           gone_up_2018 = fit>fit[year==2018]) %>% 
    group_by(state, year) %>% 
    summarise(pr_increase = mean(gone_up),
              pr_increase_2018 = mean(gone_up_2018))
  
  write_csv(res_pr_increase, paste0("results/", outcome_name, "_pr_increase_", this_race,".csv"))
  
}