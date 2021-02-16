####### Code for explorer: Projections of child welfare outcomes ##########
####### Author: Monica Alexander ##########

# 1. Load packages and data -----------------------------------------------

# Note: if packages do not exist, you can install them using install.packages("tidyverse") etc

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(maps)
library(kableExtra)
library(plotly)
library(tidybayes)
library(ggpubr)

# Data

d_res <- read_csv("results/d_res.csv")
betas <- read_csv("results/betas.csv")
pr_res <- read_csv("results/pr_res.csv")
state_div <- read_csv("inputs/state_division.csv")

# 2. Create national map --------------------------------------------------

state_map <- map_data("state")

merged_data <- inner_join(state_map, d_res %>%
  mutate(region = state) %>%
  mutate(region = str_to_lower(region)), by = "region")

Noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

# 3. Shiny code -----------------------------------------------------------

source("modules/national.R")
source("modules/state.R")
source("modules/covariates.R")
source("modules/about.R")

options(spinner.color = "#ED6F0F")

## User Interface -----

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "screen.css")
  ),
  navbarPage(
    title = "Trends and projections of foster care indicators in the United States",
    theme = bslib::bs_theme(version = 4, bg = "white", fg = "black", primary = "#ED6F0F"),


    # National tab content
    national_ui("national", d_res = d_res),

    # State tab content
    state_ui("state", d_res = d_res),

    # Covariates tab content
    covariates_ui("covariates", betas = betas),

    # About tab content
    about_ui()
  )
)

## Server -----

server <- function(input, output) {

  # National Map
  national_server("national", d_res = d_res, merged_data = merged_data, Noax = Noax)

  # State Server
  state_server("state", d_res = d_res, betas = betas, state_div = state_div, pr_res = pr_res)

  # Covariates Server
  covariates_server("covariates", betas = betas)
}

# Run the application
shinyApp(ui = ui, server = server)
