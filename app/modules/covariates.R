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
        widths = c(3, 9),
        selectInput(
          inputId = ns("indicator"), label = "Indicator",
          c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")
        ),

        selectizeInput(
          inputId = ns("covariates"),
          label = "Covariates",
          choices = betas %>%
            distinct(category, variable_description) %>%
            split(.$category) %>%
            purrr::map(~ pull(.x, variable_description)),
          selected = "Median salary of social worker",
          multiple = TRUE,
          width = "100%",
          options = list(maxItems = 9)
        )
      ),

      # horizontal_inputs(
      #   selectInput(
      #     inputId = ns("indicator"), label = "Indicator",
      #     c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")
      #   ),
      #   covariate_input("Child welfare measures", 1, selected = "Median salary of social worker"),
      #   covariate_input("Criminal justice", 2)
      # ),
      #
      # horizontal_inputs(
      #   covariate_input("Demographic measures", 3),
      #   covariate_input("Economic well-being", 4),
      #   covariate_input("Education", 5)
      # ),
      #
      # horizontal_inputs(
      #   covariate_input("Housing measures", 6),
      #   covariate_input("Public health measures", 7),
      #   covariate_input("Social support", 8)
      # ),
      hr(),

      # Show a plot of the generated distribution
      column(12,
        align = "center",
        uiOutput(ns("beta_plot_ui"))
      )
    )
  )
}

covariates_server <- function(id, betas) {
  moduleServer(
    id,
    function(input, output, session) {

      # Make vars_selected a reactive for use in plot generation and calculation of plot size
      vars_selected <- reactive(
        input$covariates
      ) %>%
        debounce(1000)

      # Render plot in reactive for reuse
      beta_plot <- reactive({
        betas %>%
          filter(
            ci_width == 0.5,
            indicator == input$indicator,
            race == "Total",
            variable_description %in% vars_selected(), year < 2019
          ) %>%
          ggplot(aes(year, value, color = variable_description)) +
          geom_point() +
          ylab("coefficient") +
          theme_bw(base_size = 14) +
          scale_x_continuous(breaks = seq(2003, 2018, by = 4)) +
          geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
          facet_wrap(~division) +
          scale_color_brewer(
            name = "covariate", palette = "Set1",
            guide = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 1)
          ) +
          theme(legend.position = "bottom")
      })

      # Grab plot only, no legend
      output$BetaPlot <- renderPlot(
        res = 96,
        {
          beta_plot()
        }
      )

      # Render plot UI, with dynamic height
      output$beta_plot_ui <- renderUI({
        withSpinner(plotOutput(session$ns("BetaPlot"), width = "80%", height = paste0(500 + (length(vars_selected()) - 1) * 25, "px")))
      })
    }
  )
}
