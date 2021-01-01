####### 1. Load and prepare data for projection models ##########
####### Author: Monica Alexander ##########

# 1. Load packages and data -----------------------------------------------

# Note: if packages do no exist, you can install them using install.packages("tidyverse") etc

library(tidyverse)
library(readxl)
source("code/functions/utils.R")


# Load in data and info

d <- read_csv("inputs/outcome_vars.csv") # outcome variables
dexp <- read_csv("inputs/explanatory_vars.csv") # possible covariates
covariates <- read_csv("inputs/covariates.csv") # list of covariates included in model


# 2. Clean up data -----------------------------------------------

# a data frame with outcomes and covariates, starting at 2000

df <- d %>% 
  left_join(dexp %>% 
              select(fips, state, year, race, all_of(covariates$variable)) %>% 
              filter(year>1999)) %>% 
  left_join(st_info %>% select(name, division) %>% rename(state = name)) %>% # add census division info
  mutate(ent_pc = ent_rate_adj/1000,
         inv_pc = invest_rate_adj/1000,
         perm_pc = perm_rate/1000,
         nperm_pc = nperm_rate/1000)


# 3. Calculate log outcomes and standard errors --------------------------------------------

df <- df %>% 
  mutate(log_ent_pc = log(ent_rate_adj/1000),
         log_inv_pc = log(invest_rate_adj/1000),
         log_perm_pc = log(perm_rate/1000),
         log_nperm_pc = log(nperm_rate/1000)) %>% 
  mutate(ent_pc_log_se = sqrt((ent_rate_adj/1000*(1-ent_rate_adj/1000)/pop)/(ent_rate_adj/1000))*sqrt(1000),
         inv_pc_log_se = sqrt((invest_rate_adj/1000*(1-invest_rate_adj/1000)/pop)/(invest_rate_adj/1000))*sqrt(1000),
         perm_pc_log_se = sqrt((perm_rate/1000*(1-perm_rate/1000)/pop)/(perm_rate/1000))*sqrt(1000),
         nperm_pc_log_se = sqrt((nperm_rate/1000*(1-nperm_rate/1000)/pop)/(nperm_rate/1000))*sqrt(1000))


# 4. Save dataframe -------------------------------------------------------

write_csv(df, path = "inputs/all_data_for_model.csv")

