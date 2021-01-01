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
                            mutate(region = str_to_lower(region)))

Noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)



# 3. Shiny code -----------------------------------------------------------


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
      # First tab content
      tabItem(tabName = "Map",
              h2("National overview"),
              fluidRow(
                box(
                  width = 10, solidHeader = FALSE, status = "primary",
                  "Select a year and indicator below to to see trends for the United States. Mouse over map to see estimate with 80% credible intervals."
                )),
              sidebarLayout(
                sidebarPanel(
                  sliderInput(inputId = 'year', label = 'Year',
                              min = 2005, #min(d_res$year),
                              max= max(d_res$year),
                              value = min(d_res$year),
                              sep = "",
                              animate = animationOptions(interval = 1000)),
                  selectInput(inputId = 'indicator', label = "Indicator",
                              c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")),
                  selectInput(inputId = 'race', label = "Race/ethnicity",
                              c("Total", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian/Pacific Islander", "Non-Hispanic American Indian/Alaska Native", "Hispanic")),
                  radioButtons(inputId = 'type_1',
                               label = 'Measure',
                               choices = c("per capita", "number"))
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotlyOutput("MapPlot")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Time",
              h2("State projections"),
              fluidRow(
                box(
                  width = 10, solidHeader = FALSE, status = "primary",
                  "Select a state and indicator below to see estimates and projections up to 2022. Shaded area indicates 80% credible intervals. The table below the chart shows the estimated probability of the indicator from year to year."
                )),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = 'state', 
                              label = 'State',
                              choices = unique(d_res$state)),
                  selectInput(inputId = 'indicator_2', label = "Indicator",
                              c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")),
                  selectInput(inputId = 'race_2', label = "Race/ethnicity",
                              c("Total", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian/Pacific Islander", "Non-Hispanic American Indian/Alaska Native", "Hispanic")),
                  radioButtons(inputId = 'type_2',
                               label = 'Measure',
                               choices = c("per capita", "number"))
                ),
                
                # State based results
                mainPanel(
                  plotlyOutput("TimePlot") , 
                  br(),
                  h4("Estimated probability of increase from year to year"),
                  tableOutput("ProbTable"),
                  h4("Top covariates influencing projection"),
                  tableOutput("CoefTable")
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "Covariates",
              h2("Estimated association with covariates"),
              fluidRow(
                box(
                  width = 10, solidHeader = FALSE, status = "primary",
                  HTML("This tab shows the coefficient estimates for each of the covariates included in the model. Select and indicator, and one or more covariates to see estimates by census division and year. Coefficients sizes represent a %-% change after controlling for all other covariates in the model.",
                       '<br/>', '<br/>',"Note: only 9 covariates can be selected at any one time.")
                )),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = 'indicator_3', label = "Indicator",
                              c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")),
                  # selectInput(inputId = 'race_3', label = "Race/ethnicity",
                  #             c("Total", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian/Pacific Islander", "Non-Hispanic American Indian/Alaska Native", "Hispanic")),
                  checkboxGroupInput(inputId = 'covariate_1', 
                                     label = 'Child welfare measures',
                                     choices = sort(unique(betas$variable_description[betas$category=="Child welfare measures"])), selected = "Median salary of social worker"),
                  checkboxGroupInput(inputId = 'covariate_2', 
                                     label = 'Criminal justice',
                                     choices = sort(unique(betas$variable_description[betas$category=="Criminal justice"]))),
                  checkboxGroupInput(inputId = 'covariate_3', 
                                     label = 'Demographic measures',
                                     choices = sort(unique(betas$variable_description[betas$category=="Demographic measures"]))),
                  checkboxGroupInput(inputId = 'covariate_4', 
                                     label = 'Economic well-being',
                                     choices = sort(unique(betas$variable_description[betas$category=="Economic well-being"]))),
                  checkboxGroupInput(inputId = 'covariate_5', 
                                     label = 'Education',
                                     choices = sort(unique(betas$variable_description[betas$category=="Education"]))),
                  checkboxGroupInput(inputId = 'covariate_6', 
                                     label = 'Housing measures',
                                     choices = sort(unique(betas$variable_description[betas$category=="Housing measures"]))),
                  checkboxGroupInput(inputId = 'covariate_7', 
                                     label = 'Public health measures',
                                     choices = sort(unique(betas$variable_description[betas$category=="Public health measures"]))),
                  checkboxGroupInput(inputId = 'covariate_8', 
                                     label = 'Social support',
                                     choices = sort(unique(betas$variable_description[betas$category=="Social support"])))
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("BetaPlot"),
                  br(),
                  plotOutput("BetaPlotLegend", inline = TRUE)
                )
              )
      ),
      
      
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
  
  output$MapPlot <- renderPlotly({
    if(input$type_1=="per capita"){
      p <- ggplot(data=merged_data %>% filter(year==input$year, 
                                              indicator==input$indicator, 
                                              race == input$race,
                                              ci_width == 0.75), 
                  aes(text=sprintf("%s<br> %s (%s, %s)", state, round(fit*1000,2), round(fit_lower*1000,2), round(fit_upper*1000,2)))) + 
        geom_polygon(aes(x=long, y=lat, group=group, fill = fit*1000), color="black") + 
        scale_fill_viridis_c(name = paste0(input$indicator, " per 1,000"), 
                             limits = c(min(d_res$fit[d_res$indicator==input$indicator&d_res$race == input$race]*1000)*0.95, 
                                        max(d_res$fit[d_res$indicator==input$indicator&d_res$race == input$race]*1000)*1)) + 
        theme_void() + 
        coord_fixed(1.5) +
        ggtitle(paste0("Foster care ",str_to_lower(input$indicator),", ", input$year, " (per 1000)\n", input$race, " population"))
      fig <- ggplotly(p, tooltip = "text") %>%
        layout(xaxis = Noax, yaxis = Noax)
      
      fig
    }
    else{
      p <- ggplot(data=merged_data %>% filter(year==input$year, 
                                              indicator==input$indicator, 
                                              race == input$race,
                                              ci_width == 0.75), 
                  aes(text=sprintf("%s<br> %s (%s, %s)", state, round(fit_pop), round(fit_lower_pop), round(fit_upper_pop)))) + 
        geom_polygon(aes(x=long, y=lat, group=group, fill = fit_pop), color="black") + 
        scale_fill_viridis_c(name = "number", 
                             limits = c(min(d_res$fit_pop[d_res$indicator==input$indicator])*0.95, 
                                        max(d_res$fit_pop[d_res$indicator==input$indicator])*1)) + 
        theme_void() + 
        coord_fixed(1.5) +
        ggtitle(paste0("Foster care ",str_to_lower(input$indicator),", ", input$year, " (number)\n", input$race, " population"))
      fig <- ggplotly(p, tooltip = "text") %>%
        layout(xaxis = Noax, yaxis = Noax)
      
      fig
    }
  })
  
  # Time Series
  
  output$TimePlot <- renderPlotly({
    if(input$type_2=="per capita"){
      p <- ggplot(data = d_res %>% filter(state == input$state, 
                                          indicator==input$indicator_2, 
                                          race == input$race_2,
                                          ci_width == 0.75), 
                  aes(year, observed*1000)) + 
        geom_line(aes(year, fit*1000, color = "estimate"))+
        geom_point(aes(color = "observed"))+
        geom_point(aes(year, fit*1000, text=sprintf("%s<br> %s (%s, %s)", 
                                                    year, round(fit*1000,2), 
                                                    round(fit_lower*1000,2), round(fit_upper*1000,2))), color = NA) +
        geom_ribbon(aes(ymin = fit_lower*1000, ymax = fit_upper*1000, fill = "estimate"), alpha = 0.2) + 
        ggtitle(paste0(input$state, " (",state_div %>% filter(state==input$state) %>% select(division) %>% pull()," division), ", input$race_2, " population")) +
        theme_bw(base_size = 14) + ylab("per 1,000") + 
        scale_fill_manual(name = "", values = c("estimate" = "red")) +
        scale_color_manual(name = "", values = c("estimate" = "red", "observed" = "black"))
      
      fig <- ggplotly(p, tooltip = "text") #%>% layout(legend = list(x = 0.1, y = 0.9))
      fig[['x']][['data']][[1]][['name']] <- 'estimate'
      fig[['x']][['data']][[2]][['name']] <- 'observed'
      
      fig 
    }
    else{
      p <- ggplot(data = d_res %>% filter(state == input$state,  indicator==input$indicator_2,  race == input$race_2, ci_width == 0.75), 
                  aes(year, observed_pop)) + 
        geom_line(aes(year, fit_pop, color = "estimate"))+
        geom_point(aes(color = "observed"))+
        geom_point(aes(year, fit_pop, text=sprintf("%s<br> %s (%s, %s)", 
                                                   year, round(fit_pop), 
                                                   round(fit_lower_pop), round(fit_upper_pop))), color = NA) +
        geom_ribbon(aes(ymin = fit_lower_pop, ymax = fit_upper_pop, fill = "estimate"), alpha = 0.2) + 
        ggtitle(paste0(input$state, " (",state_div %>% filter(state==input$state) %>% select(division) %>% pull()," division), ", input$race_2, " population")) +
        theme_bw(base_size = 14) + ylab("number") + 
        scale_fill_manual(name = "", values = c("estimate" = "red")) +
        scale_color_manual(name = "", values = c("estimate" = "red", "observed" = "black"))
      
      fig <- ggplotly(p, tooltip = "text") #%>% layout(legend = list(x = 0.1, y = 0.9))
      fig[['x']][['data']][[1]][['name']] <- 'estimate'
      fig[['x']][['data']][[2]][['name']] <- 'observed'
      
      fig
    }
  })
  output$ProbTable <- renderTable(pr_res %>% filter(state == input$state, indicator ==input$indicator_2,  race == input$race_2, year>2018) %>% 
                                    select(-state, -indicator, -race) %>% 
                                    mutate(year = as.character(year)) %>% 
                                    rename("probability of increase from previous year" = pr_increase,
                                           "probability of increase from 2018" = pr_increase_2018))
  output$CoefTable <- renderTable(betas %>% 
                                    filter(indicator==input$indicator_2,  race == input$race_2,division == state_div %>% filter(state==input$state) %>% select(division) %>% pull()) %>% 
                                    filter(year==2018) %>% 
                                    arrange(-abs(value)) %>% 
                                    mutate(sig = (lower<0&upper<0)|lower>0&upper>0,
                                           CI = paste0(" (",round(lower, 3), ",", round(upper,3),")"),
                                           estimate = round(value, 3)) %>% 
                                    select(variable_description,estimate, CI) %>% 
                                    head(5) %>% 
                                    rename(covariate = variable_description, `80% CI` = CI))
  
  # coefficients
  output$BetaPlot <- renderPlot({
    vars_selected = c(input$covariate_1, input$covariate_2,
                      input$covariate_3, input$covariate_4, 
                      input$covariate_5, input$covariate_6, 
                      input$covariate_7, input$covariate_8)
    p <-  betas %>% 
      filter(ci_width ==0.5, 
             indicator == input$indicator_3,
             race == "Total",
             variable_description %in% vars_selected, year<2019) %>% 
      ggplot(aes(year, value, color = variable_description)) + 
      geom_point() + 
      ylab("coefficient") + 
      theme_bw(base_size = 14)+
      scale_x_continuous(breaks = seq(2003, 2018, by= 4))+
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)+
      facet_wrap(~division) +
      scale_color_brewer(name = "covariate", palette = "Set1")+
      theme(legend.position="bottom", legend.direction="vertical")
    p_leg <- get_legend(p)
    p+theme(legend.position = 'none')
  }) #, height = 500, width = 700
  output$BetaPlotLegend <- renderPlot({
    vars_selected = c(input$covariate_1, input$covariate_2,
                      input$covariate_3, input$covariate_4, 
                      input$covariate_5, input$covariate_6, 
                      input$covariate_7, input$covariate_8)
    p <- betas %>% 
      filter(ci_width ==0.5, variable_description %in% vars_selected) %>% 
      ggplot(aes(year, value, color = variable_description)) + 
      geom_point() + 
      ylab("coefficient") + 
      theme_bw(base_size = 14)+
      scale_x_continuous(breaks = seq(2010, 2016, by= 2))+
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)+
      facet_wrap(~division) +
      scale_color_brewer(name = "covariate", palette = "Set1")+
      theme(legend.position="bottom", legend.direction="vertical")
    p_leg <- get_legend(p)
    as_ggplot(p_leg)
  },height = 200, width = 400) 
  output$BetaTable <- renderTable(betas %>% 
                                    group_by(division,variable_description) %>% 
                                    summarise(mean = mean(value))  %>% 
                                    arrange(division, -abs(mean)) %>% 
                                    group_by(division) %>% 
                                    slice(1:5) %>% 
                                    rename('covariate' = variable_description, 
                                           'average effect' = mean) 
  )
  
  # scatter
  output$ScatterPlot <- renderPlot({
    covar <- betas %>% filter(variable_description==input$scatter) %>% select(covariate_name) %>% slice(1) %>% pull()
    if(input$groups==TRUE){
      p <- d_data %>% 
        rename(this_c = covar) %>% 
        ggplot(aes(this_c, ent_pc, color = division)) + 
        geom_point() + scale_y_log10() + 
        scale_x_log10() +
        ylab("entries per 1,000") + xlab(input$scatter)+
        theme_bw(base_size = 14)+
        scale_color_viridis_d()+
        ggtitle(paste("Entries per capita versus", input$scatter))
    }
    else{
      p <- d_data %>% 
        rename(this_c = covar) %>% 
        ggplot(aes(this_c, ent_pc)) + 
        geom_point() + scale_y_log10() + 
        scale_x_log10() +
        ylab("entries per 1,000") + xlab(input$scatter)+
        theme_bw(base_size = 14)+
        ggtitle(paste0("Entries per capita versus \n", input$scatter))
    }
    if(input$fitted_line==TRUE){
      p+geom_smooth(method = "lm")
    }
    else{
      p
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)