####### 3. Prepare results for app ##########
####### Author: Monica Alexander ##########

library(tidyverse)


# 1. Load in all the results and bind races together ----------------------------------------------

## Entries

d_res_entries <- read_csv("results/entries_obs_fit_total.csv") %>% mutate(indicator = "Entries", race = "Total")
betas_entries <- read_csv("results/betas_entries_total.csv")  %>% mutate(indicator = "Entries", race = "Total")
pr_res_entries <- read_csv("results/entries_pr_increase_total.csv")  %>% mutate(indicator = "Entries", race = "Total")

d_res_entries_1 <- read_csv("results/entries_obs_fit_white.csv") %>% mutate(indicator = "Entries", race = "Non-Hispanic White")
betas_entries_1 <- read_csv("results/betas_entries_white.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic White")
pr_res_entries_1 <- read_csv("results/entries_pr_increase_white.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic White")

d_res_entries_2 <- read_csv("results/entries_obs_fit_black.csv") %>% mutate(indicator = "Entries", race = "Non-Hispanic Black")
betas_entries_2 <- read_csv("results/betas_entries_black.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic Black")
pr_res_entries_2 <- read_csv("results/entries_pr_increase_black.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic Black")

d_res_entries_3 <- read_csv("results/entries_obs_fit_aapi.csv") %>% mutate(indicator = "Entries", race = "Non-Hispanic Asian/Pacific Islander")
betas_entries_3 <- read_csv("results/betas_entries_aapi.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic Asian/Pacific Islander")
pr_res_entries_3 <- read_csv("results/entries_pr_increase_aapi.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic Asian/Pacific Islander")

d_res_entries_4 <- read_csv("results/entries_obs_fit_aian.csv") %>% mutate(indicator = "Entries", race = "Non-Hispanic American Indian/Alaska Native")
betas_entries_4 <- read_csv("results/betas_entries_aian.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic American Indian/Alaska Native")
pr_res_entries_4 <- read_csv("results/entries_pr_increase_aian.csv")  %>% mutate(indicator = "Entries", race = "Non-Hispanic American Indian/Alaska Native")

d_res_entries_5 <- read_csv("results/entries_obs_fit_hispanic.csv") %>% mutate(indicator = "Entries", race = "Hispanic")
betas_entries_5 <- read_csv("results/betas_entries_hispanic.csv")  %>% mutate(indicator = "Entries", race = "Hispanic")
pr_res_entries_5 <- read_csv("results/entries_pr_increase_hispanic.csv")  %>% mutate(indicator = "Entries", race = "Hispanic")

# Bind them together

d_res_entries_all <- d_res_entries %>% 
  bind_rows(d_res_entries_1) %>% 
  bind_rows(d_res_entries_2) %>% 
  bind_rows(d_res_entries_3) %>% 
  bind_rows(d_res_entries_4) %>% 
  bind_rows(d_res_entries_5) 

betas_entries_all <-  betas_entries %>% 
  bind_rows(betas_entries_1) %>% 
  bind_rows(betas_entries_2) %>% 
  bind_rows(betas_entries_3) %>% 
  bind_rows(betas_entries_4) %>% 
  bind_rows(betas_entries_5)

pr_res_entries_all <- pr_res_entries %>%
  bind_rows(pr_res_entries_1) %>% 
  bind_rows(pr_res_entries_2) %>% 
  bind_rows(pr_res_entries_3) %>% 
  bind_rows(pr_res_entries_4) %>% 
  bind_rows(pr_res_entries_5) 

## Investigations

d_res_inv <- read_csv("results/invest_obs_fit_total.csv") %>% mutate(indicator = "Investigations", race = "Total")
betas_inv <- read_csv("results/betas_invest_total.csv")  %>% mutate(indicator = "Investigations", race = "Total")
pr_res_inv <- read_csv("results/invest_pr_increase_total.csv")  %>% mutate(indicator = "Investigations", race = "Total")


d_res_inv_1 <- read_csv("results/invest_obs_fit_white.csv") %>% mutate(indicator = "Investigations", race = "Non-Hispanic White")
betas_inv_1 <- read_csv("results/betas_invest_white.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic White")
pr_res_inv_1 <- read_csv("results/invest_pr_increase_white.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic White")

d_res_inv_2 <- read_csv("results/invest_obs_fit_black.csv") %>% mutate(indicator = "Investigations", race = "Non-Hispanic Black")
betas_inv_2 <- read_csv("results/betas_invest_black.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic Black")
pr_res_inv_2 <- read_csv("results/invest_pr_increase_black.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic Black")


d_res_inv_3 <- read_csv("results/invest_obs_fit_aapi.csv") %>% mutate(indicator = "Investigations", race = "Non-Hispanic Asian/Pacific Islander")
betas_inv_3 <- read_csv("results/betas_invest_aapi.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic Asian/Pacific Islander")
pr_res_inv_3 <- read_csv("results/invest_pr_increase_aapi.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic Asian/Pacific Islander")


d_res_inv_4 <- read_csv("results/invest_obs_fit_aian.csv") %>% mutate(indicator = "Investigations", race = "Non-Hispanic American Indian/Alaska Native")
betas_inv_4 <- read_csv("results/betas_invest_aian.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic American Indian/Alaska Native")
pr_res_inv_4 <- read_csv("results/invest_pr_increase_aian.csv")  %>% mutate(indicator = "Investigations", race = "Non-Hispanic American Indian/Alaska Native")


d_res_inv_5 <- read_csv("results/invest_obs_fit_hispanic.csv") %>% mutate(indicator = "Investigations", race = "Hispanic")
betas_inv_5 <- read_csv("results/betas_invest_hispanic.csv")  %>% mutate(indicator = "Investigations", race = "Hispanic")
pr_res_inv_5 <- read_csv("results/invest_pr_increase_hispanic.csv")  %>% mutate(indicator = "Investigations", race = "Hispanic")

# Bind them together

d_res_inv_all <- d_res_inv %>% 
  bind_rows(d_res_inv_1) %>% 
  bind_rows(d_res_inv_2) %>% 
  bind_rows(d_res_inv_3) %>% 
  bind_rows(d_res_inv_4) %>% 
  bind_rows(d_res_inv_5) 

betas_inv_all <-  betas_inv %>% 
  bind_rows(betas_inv_1) %>% 
  bind_rows(betas_inv_2) %>% 
  bind_rows(betas_inv_3) %>% 
  bind_rows(betas_inv_4) %>% 
  bind_rows(betas_inv_5)

pr_res_inv_all <- pr_res_inv %>%
  bind_rows(pr_res_inv_1) %>% 
  bind_rows(pr_res_inv_2) %>% 
  bind_rows(pr_res_inv_3) %>% 
  bind_rows(pr_res_inv_4) %>% 
  bind_rows(pr_res_inv_5) 

## Permanent

d_res_perm <- read_csv("results/perm_obs_fit_total.csv") %>% mutate(indicator = "Permanent exits", race = "Total")
betas_perm <- read_csv("results/betas_perm_total.csv")  %>% mutate(indicator = "Permanent exits", race = "Total")
pr_res_perm <- read_csv("results/perm_pr_increase_total.csv")  %>% mutate(indicator = "Permanent exits", race = "Total")


d_res_perm_1 <- read_csv("results/perm_obs_fit_white.csv") %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic White")
betas_perm_1 <- read_csv("results/betas_perm_white.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic White")
pr_res_perm_1 <- read_csv("results/perm_pr_increase_white.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic White")


d_res_perm_2 <- read_csv("results/perm_obs_fit_black.csv") %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Black")
betas_perm_2 <- read_csv("results/betas_perm_black.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Black")
pr_res_perm_2 <- read_csv("results/perm_pr_increase_black.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Black")


d_res_perm_3 <- read_csv("results/perm_obs_fit_aapi.csv") %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Asian/Pacific Islander")
betas_perm_3 <- read_csv("results/betas_perm_aapi.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Asian/Pacific Islander")
pr_res_perm_3 <- read_csv("results/perm_pr_increase_aapi.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic Asian/Pacific Islander")


d_res_perm_4 <- read_csv("results/perm_obs_fit_aian.csv") %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic American Indian/Alaska Native")
betas_perm_4 <- read_csv("results/betas_perm_aian.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic American Indian/Alaska Native")
pr_res_perm_4 <- read_csv("results/perm_pr_increase_aian.csv")  %>% mutate(indicator = "Permanent exits", race = "Non-Hispanic American Indian/Alaska Native")


d_res_perm_5 <- read_csv("results/perm_obs_fit_hispanic.csv") %>% mutate(indicator = "Permanent exits", race = "Hispanic")
betas_perm_5 <- read_csv("results/betas_perm_hispanic.csv")  %>% mutate(indicator = "Permanent exits", race = "Hispanic")
pr_res_perm_5 <- read_csv("results/perm_pr_increase_hispanic.csv")  %>% mutate(indicator = "Permanent exits", race = "Hispanic")

# Bind them together

d_res_perm_all <- d_res_perm %>% 
  bind_rows(d_res_perm_1) %>% 
  bind_rows(d_res_perm_2) %>% 
  bind_rows(d_res_perm_3) %>% 
  bind_rows(d_res_perm_4) %>% 
  bind_rows(d_res_perm_5) 

betas_perm_all <-  betas_perm %>% 
  bind_rows(betas_perm_1) %>% 
  bind_rows(betas_perm_2) %>% 
  bind_rows(betas_perm_3) %>% 
  bind_rows(betas_perm_4) %>% 
  bind_rows(betas_perm_5)

pr_res_perm_all <- pr_res_perm %>%
  bind_rows(pr_res_perm_1) %>% 
  bind_rows(pr_res_perm_2) %>% 
  bind_rows(pr_res_perm_3) %>% 
  bind_rows(pr_res_perm_4) %>% 
  bind_rows(pr_res_perm_5) 

# Non-permanent exits

d_res_nperm <- read_csv("results/nperm_obs_fit_total.csv") %>% mutate(indicator = "Non-permanent exits", race = "Total")
betas_nperm <- read_csv("results/betas_nperm_total.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Total")
pr_res_nperm <- read_csv("results/nperm_pr_increase_total.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Total")


d_res_nperm_1 <- read_csv("results/nperm_obs_fit_white.csv") %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic White")
betas_nperm_1 <- read_csv("results/betas_nperm_white.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic White")
pr_res_nperm_1 <- read_csv("results/nperm_pr_increase_white.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic White")


d_res_nperm_2 <- read_csv("results/nperm_obs_fit_black.csv") %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Black")
betas_nperm_2 <- read_csv("results/betas_nperm_black.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Black")
pr_res_nperm_2 <- read_csv("results/nperm_pr_increase_black.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Black")


d_res_nperm_3 <- read_csv("results/nperm_obs_fit_aapi.csv") %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Asian/Pacific Islander")
betas_nperm_3 <- read_csv("results/betas_nperm_aapi.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Asian/Pacific Islander")
pr_res_nperm_3 <- read_csv("results/nperm_pr_increase_aapi.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic Asian/Pacific Islander")


d_res_nperm_4 <- read_csv("results/nperm_obs_fit_aian.csv") %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic American Indian/Alaska Native")
betas_nperm_4 <- read_csv("results/betas_nperm_aian.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic American Indian/Alaska Native")
pr_res_nperm_4 <- read_csv("results/nperm_pr_increase_aian.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Non-Hispanic American Indian/Alaska Native")


d_res_nperm_5 <- read_csv("results/nperm_obs_fit_hispanic.csv") %>% mutate(indicator = "Non-permanent exits", race = "Hispanic")
betas_nperm_5 <- read_csv("results/betas_nperm_hispanic.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Hispanic")
pr_res_nperm_5 <- read_csv("results/nperm_pr_increase_hispanic.csv")  %>% mutate(indicator = "Non-permanent exits", race = "Hispanic")

#  Bind them together

d_res_nperm_all <- d_res_nperm %>% 
  bind_rows(d_res_nperm_1) %>% 
  bind_rows(d_res_nperm_2) %>% 
  bind_rows(d_res_nperm_3) %>% 
  bind_rows(d_res_nperm_4) %>% 
  bind_rows(d_res_nperm_5) 

betas_nperm_all <-  betas_nperm %>% 
  bind_rows(betas_nperm_1) %>% 
  bind_rows(betas_nperm_2) %>% 
  bind_rows(betas_nperm_3) %>% 
  bind_rows(betas_nperm_4) %>% 
  bind_rows(betas_nperm_5)

pr_res_nperm_all <- pr_res_nperm %>%
  bind_rows(pr_res_nperm_1) %>% 
  bind_rows(pr_res_nperm_2) %>% 
  bind_rows(pr_res_nperm_3) %>% 
  bind_rows(pr_res_nperm_4) %>% 
  bind_rows(pr_res_nperm_5) 


# 2. Bind all outcomes together and save in app folder --------------------

d_res <- d_res_entries_all %>% bind_rows(d_res_inv_all) %>% bind_rows(d_res_perm_all) %>% bind_rows(d_res_nperm_all)
betas <- betas_entries_all %>% bind_rows(betas_inv_all)%>% bind_rows(betas_perm_all) %>% bind_rows(betas_nperm_all)
pr_res <- pr_res_entries_all %>% bind_rows(pr_res_inv_all) %>% bind_rows(pr_res_perm_all) %>% bind_rows(pr_res_nperm_all)

d_res <- bind_rows(d_res %>% filter(year<2019,ci_width == 0.75), d_res %>% filter(year>=2019,ci_width == 0.5)) %>% mutate(ci_width=0.75)
betas <- betas %>% filter(ci_width == 0.5)

write_csv(d_res, "app/results/d_res.csv")
write_csv(betas, "app/results/betas.csv")
write_csv(pr_res, "app/results/pr_res.csv")




