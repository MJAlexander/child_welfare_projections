####### 2. Run projection models ##########
####### Author: Monica Alexander ##########

# 1. Load packages and data -----------------------------------------------

# Note: if packages do no exist, you can install them using install.packages("tidyverse") etc

library(tidyverse)
library(rstan)
library(tidybayes)

df <- read_csv("inputs/all_data_for_model.csv", 
               col_types = cols(turnover_casewkr_lw = col_double(), 
                                turnover_casewkr_up = col_double()))
covariates <- read_csv("inputs/covariates.csv") # list of covariates included in model

# load in functions to run stan
source("code/functions/prepare_stan_data.R")
source("code/functions/write_save_model_results.R")


# 2. Set global variables ---------------------------------------------------

# year-related variables

years <- 2005:2018 # years of data
P <- 5 # number of projection steps
years_all <- c(years, years[length(years)]+1:P) # all years
years_proj <- c(years[length(years)]+1:P) # projection years
years_c <- years_all - mean(years) # mean-centered years

states <- unique(df$state)
regions <- sort(unique(df$division))
races <- unique(df$race)

# create a list of meta data
meta_data <- list(years = years,
                  P = P,
                  years_all = years_all,
                  years_proj = years_proj, 
                  years_c = years_c,
                  states = states,
                  regions = regions,
                  races = races)


# 3. Run entries models ---------------------------------------------------


for(this_race in races){
  
  # get data into stan form
  this_data <- prepare_stan_data(df = df, 
                                 covariates = covariates, this_race = this_race, 
                                 meta_data = meta_data, 
                                 outcome_col = "log_ent_pc", se_col = "ent_pc_log_se")
  
  # run the model
  mod <- stan(data = this_data$stan_data, 
              file = "code/models/child_welfare_proj.stan",
              iter = 400,
              chains = 3,
              control = list(adapt_delta = 0.8))
  
  
  # write and save the results
  
  write_save_model_results(mod = mod, dfr = this_data$dfr,
                           stan_data = this_data$stan_data,
                           covariates = covariates,
                           this_race = this_race, 
                           meta_data = meta_data, 
                           outcome_col = "ent_pc", outcome_name = "entries")
  
}


# 4. Run investigations models --------------------------------------------

for(this_race in races){
  
  # get data into stan form
  this_data <- prepare_stan_data(df = df, 
                                 covariates = covariates, this_race = this_race, 
                                 meta_data = meta_data, 
                                 outcome_col = "log_inv_pc", se_col = "inv_pc_log_se")
  
  # run the model
  mod <- stan(data = this_data$stan_data, 
              file = "code/models/child_welfare_proj.stan",
              iter = 400,
              chains = 3,
              control = list(adapt_delta = 0.8))
  
  
  # write and save the results
  
  write_save_model_results(mod = mod, dfr = this_data$dfr,
                           stan_data = this_data$stan_data,
                           covariates = covariates,
                           this_race = this_race, 
                           meta_data = meta_data, 
                           outcome_col = "inv_pc", outcome_name = "invest")
  
}


# 5. Run permanent exits models -------------------------------------------

for(this_race in races){
  
  # get data into stan form
  this_data <- prepare_stan_data(df = df, 
                                 covariates = covariates, this_race = this_race, 
                                 meta_data = meta_data, 
                                 outcome_col = "log_perm_pc", se_col = "perm_pc_log_se")
  
  # run the model
  mod <- stan(data = this_data$stan_data, 
              file = "code/models/child_welfare_proj.stan",
              iter = 400,
              chains = 3,
              control = list(adapt_delta = 0.8))
  
  
  # write and save the results
  
  write_save_model_results(mod = mod, dfr = this_data$dfr,
                           stan_data = this_data$stan_data,
                           covariates = covariates,
                           this_race = this_race, 
                           meta_data = meta_data, 
                           outcome_col = "perm_pc", outcome_name = "perm")
  
}


# 6. Run non-permanent exits models -------------------------------------------

for(this_race in races){
  
  # get data into stan form
  this_data <- prepare_stan_data(df = df, 
                                 covariates = covariates, this_race = this_race, 
                                 meta_data = meta_data, 
                                 outcome_col = "log_nperm_pc", se_col = "nperm_pc_log_se")
  
  # run the model
  mod <- stan(data = this_data$stan_data, 
              file = "code/models/child_welfare_proj.stan",
              iter = 400,
              chains = 3,
              control = list(adapt_delta = 0.8))
  
  
  # write and save the results
  
  write_save_model_results(mod = mod, dfr = this_data$dfr,
                           stan_data = this_data$stan_data,
                           covariates = covariates,
                           this_race = this_race, 
                           meta_data = meta_data, 
                           outcome_col = "nperm_pc", outcome_name = "nperm")
  
}




