covariates_ui <- function(id, betas) {
  ns <- shiny::NS(id)

  # Function to create covariate selectors
  covariate_input <- function(label, i, selected = NULL) {
    selectizeInput(
      multiple = TRUE,
      selected = selected,
      inputId = ns(paste0("covariate_", i)),
      label = label,
      choices = sort(unique(betas$variable_description[betas$category == label]))
    )
  }

  tabPanel(
    "Covariates",
    div(
      class = "app-page",
      h2("Estimated association with covariates"),
      fluidRow(
        class = "app-row",
        p("This tab shows the coefficient estimates for each of the covariates included in the model. Select an indicator, and one or more covariates to see estimates by census division and year. Coefficients sizes represent a %-% change after controlling for all other covariates in the model."),
        p("Note: only 9 covariates can be selected at any one time.")
      ),
      hr(),

      horizontal_inputs(
        selectInput(
          inputId = ns("indicator"), label = "Indicator",
          c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")
        ),
        covariate_input("Child welfare measures", 1, selected = "Median salary of social worker"),
        covariate_input("Criminal justice", 2)
      ),

      horizontal_inputs(
        covariate_input("Demographic measures", 3),
        covariate_input("Economic well-being", 4),
        covariate_input("Education", 5)
      ),

      horizontal_inputs(
        covariate_input("Housing measures", 6),
        covariate_input("Public health measures", 7),
        covariate_input("Social support", 8)
      ),
      hr(),

      # Show a plot of the generated distribution
      column(12,
        align = "center",
        withSpinner(plotOutput(ns("BetaPlot"), width = "80%", height = "500px")),
        plotOutput(ns("BetaPlotLegend"), height = "100px")
      )
    )
  )
}

covariates_server <- function(id, betas) {
  moduleServer(
    id,
    function(input, output, session) {

      beta_plot <- reactive({
        vars_selected <- c(
          input$covariate_1, input$covariate_2,
          input$covariate_3, input$covariate_4,
          input$covariate_5, input$covariate_6,
          input$covariate_7, input$covariate_8
        )

        betas %>%
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
      })

      output$BetaPlot <- renderPlot(
        res = 96,
        {
          beta_plot() +
            theme(legend.position = "none")
        }
      )

      output$BetaPlotLegend <- renderPlot(
        res = 96,
        {
          p_leg <- get_legend(beta_plot())
          as_ggplot(p_leg)
        }
      )
    }
  )
}
