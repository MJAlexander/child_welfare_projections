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
- `2_run_models` runs projection models for all outcome and race/ethnicity combinations and saves the results to the `results` folder and also within the `app` folder. 


If there have been updates to the data, the covariates included in the model, these scripts should be run in numerical order. 

The remainder of the scripts in the `scripts` folder are either helper functions or the model code. 

## A note on the model

The statistical model is written in the Stan programming langauge, and the file is contained in the `code/models` folder. Changes to the model will require editing this file and probably also the two functions (contained in the `code/functions` folder), `prepare_stan_data` and `write_save_model_results`.







