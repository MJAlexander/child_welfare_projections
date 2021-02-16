covariates_ui <- function(id, betas) {
  ns <- shiny::NS(id)


  tabPanel(
    "Covariates",
    div(
      class = "app-page",
      h2("Estimated association with covariates"),
      fluidRow(
        box(
          width = 10, solidHeader = FALSE, status = "primary",
          HTML(
            "This tab shows the coefficient estimates for each of the covariates included in the model. Select and indicator, and one or more covariates to see estimates by census division and year. Coefficients sizes represent a %-% change after controlling for all other covariates in the model.",
            "<br/>", "<br/>", "Note: only 9 covariates can be selected at any one time."
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = ns("indicator"), label = "Indicator",
            c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")
          ),
          checkboxGroupInput(
            inputId = ns("covariate_1"),
            label = "Child welfare measures",
            choices = sort(unique(betas$variable_description[betas$category == "Child welfare measures"])), selected = "Median salary of social worker"
          ),
          checkboxGroupInput(
            inputId = ns("covariate_2"),
            label = "Criminal justice",
            choices = sort(unique(betas$variable_description[betas$category == "Criminal justice"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_3"),
            label = "Demographic measures",
            choices = sort(unique(betas$variable_description[betas$category == "Demographic measures"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_4"),
            label = "Economic well-being",
            choices = sort(unique(betas$variable_description[betas$category == "Economic well-being"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_5"),
            label = "Education",
            choices = sort(unique(betas$variable_description[betas$category == "Education"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_6"),
            label = "Housing measures",
            choices = sort(unique(betas$variable_description[betas$category == "Housing measures"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_7"),
            label = "Public health measures",
            choices = sort(unique(betas$variable_description[betas$category == "Public health measures"]))
          ),
          checkboxGroupInput(
            inputId = ns("covariate_8"),
            label = "Social support",
            choices = sort(unique(betas$variable_description[betas$category == "Social support"]))
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          withSpinner(plotOutput(ns("BetaPlot"))),
          br(),
          plotOutput(ns("BetaPlotLegend"), inline = TRUE)
        )
      )
    )
  )
}

covariates_server <- function(id, betas) {
  moduleServer(
    id,
    function(input, output, session) {

      # coefficients
      output$BetaPlot <- renderPlot({
        vars_selected <- c(
          input$covariate_1, input$covariate_2,
          input$covariate_3, input$covariate_4,
          input$covariate_5, input$covariate_6,
          input$covariate_7, input$covariate_8
        )
        p <- betas %>%
          filter(
            ci_width == 0.5,
            indicator == input$indicator,
            race == "Total",
            variable_description %in% vars_selected, year < 2019
          ) %>%
          ggplot(aes(year, value, color = variable_description)) +
          geom_point() +
          ylab("coefficient") +
          theme_bw(base_size = 14) +
          scale_x_continuous(breaks = seq(2003, 2018, by = 4)) +
          geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
          facet_wrap(~division) +
          scale_color_brewer(name = "covariate", palette = "Set1") +
          theme(legend.position = "bottom", legend.direction = "vertical")
        p_leg <- get_legend(p)
        p + theme(legend.position = "none")
      }) # , height = 500, width = 700
      output$BetaPlotLegend <- renderPlot(
        {
          vars_selected <- c(
            input$covariate_1, input$covariate_2,
            input$covariate_3, input$covariate_4,
            input$covariate_5, input$covariate_6,
            input$covariate_7, input$covariate_8
          )
          p <- betas %>%
            filter(ci_width == 0.5, variable_description %in% vars_selected) %>%
            ggplot(aes(year, value, color = variable_description)) +
            geom_point() +
            ylab("coefficient") +
            theme_bw(base_size = 14) +
            scale_x_continuous(breaks = seq(2010, 2016, by = 2)) +
            geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
            facet_wrap(~division) +
            scale_color_brewer(name = "covariate", palette = "Set1") +
            theme(legend.position = "bottom", legend.direction = "vertical")
          p_leg <- get_legend(p)
          as_ggplot(p_leg)
        },
        height = 200,
        width = 400
      )
      output$BetaTable <- renderTable(betas %>%
        group_by(division, variable_description) %>%
        summarise(mean = mean(value)) %>%
        arrange(division, -abs(mean)) %>%
        group_by(division) %>%
        slice(1:5) %>%
        rename(
          "covariate" = variable_description,
          "average effect" = mean
        ))

      # scatter
      output$ScatterPlot <- renderPlot({
        covar <- betas %>%
          filter(variable_description == input$scatter) %>%
          select(covariate_name) %>%
          slice(1) %>%
          pull()
        if (input$groups == TRUE) {
          p <- d_data %>%
            rename(this_c = covar) %>%
            ggplot(aes(this_c, ent_pc, color = division)) +
            geom_point() +
            scale_y_log10() +
            scale_x_log10() +
            ylab("entries per 1,000") +
            xlab(input$scatter) +
            theme_bw(base_size = 14) +
            scale_color_viridis_d() +
            ggtitle(paste("Entries per capita versus", input$scatter))
        }
        else {
          p <- d_data %>%
            rename(this_c = covar) %>%
            ggplot(aes(this_c, ent_pc)) +
            geom_point() +
            scale_y_log10() +
            scale_x_log10() +
            ylab("entries per 1,000") +
            xlab(input$scatter) +
            theme_bw(base_size = 14) +
            ggtitle(paste0("Entries per capita versus \n", input$scatter))
        }
        if (input$fitted_line == TRUE) {
          p + geom_smooth(method = "lm")
        }
        else {
          p
        }
      })
    }
  )
}
