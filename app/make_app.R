####### Code for explorer: Projections of child welfare outcomes ##########
####### Author: Monica Alexander ##########

# 1. Load packages and data -----------------------------------------------

# Note: if packages do no exist, you can install them using install.packages("tidyverse") etc

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(maps)
library(kableExtra)
library(plotly)
library(tidybayes)
library(ggpubr)


# data

d_res <- read_csv("results/d_res.csv")
betas <- read_csv("results/betas.csv")
pr_res <- read_csv("results/pr_res.csv")
state_div <- read_csv("inputs/state_division.csv")


# 2. create national map --------------------------------------------------


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

fs::dir_ls("modules") %>%
  walk(source)

## user interface

ui <- dashboardPage(
  dashboardHeader(title = "Trends and projections of foster care indicators in the United States", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("National", tabName = "Map", icon = icon("globe")),
      menuItem("State", tabName = "Time", icon = icon("th")),
      menuItem("Covariates", tabName = "Covariates", icon = icon("users")),
      menuItem("About", tabName = "About", icon = icon("info"))
    )
  ),
  dashboardBody(
    setBackgroundColor(color = "white", shinydashboard = TRUE),
    tabItems(
      # National tab content
      national_ui("national", d_res = d_res),

      # State tab content
      # state_ui("state", d_res = d_res),

      # Covariates tab content
      # covariates_ui("state", betas = betas),



      # Final tab content
      tabItem(tabName = "About",
              h2("About this project"),
              fluidRow(
                box(
                  width = 9, solidHeader = FALSE, status = "primary",
                  HTML("This app shows the preliminary results of a projection model of foster care outcomes by state in the United States.",
                       '<br/>', '<br/>', "The projections are based on a Bayesian hirerchical state space model. In brief, foster care entries are modeled as a function of a set of demographic, socioeconomic, health and welfare variables (shown in the 'Covariates' tab). The association between each of the variables is allowed to vary across geography (census division) and time.",
                       '<br/>', '<br/>', "Code can be found at: https://github.com/MJAlexander/child_welfare_projections.")
                )))
    )
  )
)

#### Server ####
server <- function(input, output) {

  # National Map
  callModule(national_server, "national", d_res = d_res, merged_data = merged_data, Noax = Noax)

  # State Server
  # callModule(state_server, "state", d_res = d_res, betas = betas)

  # Covariates Server
  # callModule(covariates_server, "covariates", betas = betas)

}

# Run the application
shinyApp(ui = ui, server = server)
