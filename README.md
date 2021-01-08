# Forecasting child welfare outcomes in the United States

This repository contains code to run projection models for the four following outcomes:

- Entries into protective care
- Children involved in maltreatment investigation
- Exits from protective care with permanency
- Exits from protective care without permanency

The outcomes are projected for six race/ethnicity groups

- Non-hispanic white population
- Non-hispanic black population
- Non-hispanic Asian/Pacific Islander population
- Non-hispanic American Indian/Alaskan Native population
- Total population

The repository also contains code to create the Shiny application to explore projection results, currently hosted at: https://monica-alexander.shinyapps.io/foster_care/


# Inputs

In the `inputs` folder there are several used to get data for the projection models. 

- `explanatory_vars` contains all potential covariates. This is created using the data processing scripts (written by Alex)
- `outcome_vars` contains all outcomes. This is created using the data processing scripts (written by Alex)
- `covariates` contains a list and description of covariates included in the model. This was manually created based on exploratory data analysis, discussions with Casey and model selection. 
- `all_data_for_model` contains all data required to run projection models and is an output of the script `1_load_prepare_data` (see below)

Notes on data updates:

- If there are data updates based on rerunning Alex's scripts then `explanatory_vars` and `outcome_vars` should be directly replaced.
- If updates to the list of covariates included in the model are desired, then `covariates` should be updated.
- If any of the above data files are updated, then `all_data_for_model` should be recreated (see below).


# Code

In the `code` folder, the main scripts that need to be executed are prefixed with numbers. 

- `1_load_prepare_data` processes the data to create a file for modeling (called `all_data_for_model`). If there have been updates to the data, this script needs to be run. 
- `2_run_models` runs projection models for all outcome and race/ethnicity combinations and saves the results to the `results` folder. 
- `3_prepare_results_for_app` reads in the results and prepares them for the Shiny app, saving the output too the `app` folder. 

If there have been updates to the data, the covariates included in the model, these scripts should be run in numerical order. 

The remainder of the scripts in the `scripts` folder are either helper functions or the model code. 

## Changing the years of the model run

Currently, the model is set to run using data from 2005 to 2018, with a projection period of 5 years. If these years need to be changed, then lines 26 and 27 of the `2_run_models` should be edited. For example, if 2019 is made available and is updated in the data files, the line 26 should change to 

```
years <- 2005:2019
```

If you would like to project forward 7 years instead of 5 years, for example, then line 27 becomes

```
P <- 7
```

## A note on the model

The statistical model is written in the Stan programming langauge, and the file is contained in the `code/models` folder. Changes to the model will require editing this file and probably also the two functions (contained in the `code/functions` folder), `prepare_stan_data` and `write_save_model_results`. This would take a more advanced knowledge of R and Stan compared to making data updates. 

# Shiny App

All code and inputs required to make the Shiny App are contained in the `app` folder. To run the app, open the `make_app` script and click the "Run App" button that is display at the top of the screen in RStudio. This runs the app locally. To publish the app (e.g. on the shinyapps.io website), click the "Publish" button on top of the app. If there have been data or model updated you will need to rerun and republish the Shiny app. 





