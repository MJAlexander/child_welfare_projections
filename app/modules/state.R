state_ui <- function(id, d_res) {
  ns <- shiny::NS(id)


  tabPanel(
    "State",
    div(
      class = "app-page",
      h2("State projections"),
      fluidRow(
        class = "app-row",
        "Select a state and indicator below to see estimates and projections up to 2022. Shaded area indicates 80% credible intervals. The table below the chart shows the estimated probability of the indicator from year to year."
      ),
      hr(),
      fluidRow(
        class = "app-row",
        column(
          width = 3,
          selectInput(
            inputId = ns("state"),
            label = "State",
            choices = unique(d_res$state)
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = ns("indicator"), label = "Indicator",
            c("Entries", "Permanent exits", "Non-permanent exits", "Investigations")
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = ns("race"), label = "Race/ethnicity",
            c("Total", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian/Pacific Islander", "Non-Hispanic American Indian/Alaska Native", "Hispanic")
          )
        ),

        column(
          width = 3,
          radioButtons(
            inputId = ns("type"),
            label = "Measure",
            choices = c("per capita", "number")
          )
        )
      ),
      hr(),
        withSpinner(plotlyOutput(ns("TimePlot"), width = "100%", height = "400px")),

      fluidRow(
        class = "app-row",
        column(
          width = 6,
          h4("Estimated probability of increase from year to year"),
          tableOutput(ns("ProbTable"))
        ),
        column(
          width = 6,
          h4("Top covariates influencing projection"),
          tableOutput(ns("CoefTable"))
        )
      )
    )
  )
}


state_server <- function(id, d_res, betas, state_div, pr_res) {
  moduleServer(
    id,
    function(input, output, session) {

      output$TimePlot <- renderPlotly({
        if (input$type == "per capita") {
          p <- ggplot(
            data = d_res %>% filter(
              state == input$state,
              indicator == input$indicator,
              race == input$race,
              ci_width == 0.75
            ),
            aes(year, observed * 1000)
          ) +
            geom_line(aes(year, fit * 1000, color = "estimate")) +
            geom_point(aes(color = "observed")) +
            geom_point(aes(year, fit * 1000, text = sprintf(
              "%s<br> %s (%s, %s)",
              year, round(fit * 1000, 2),
              round(fit_lower * 1000, 2), round(fit_upper * 1000, 2)
            )), color = NA) +
            geom_ribbon(aes(ymin = fit_lower * 1000, ymax = fit_upper * 1000, fill = "estimate"), alpha = 0.2) +
            ggtitle(paste0(input$state, " (", state_div %>% filter(state == input$state) %>% select(division) %>% pull(), " division), ", input$race, " population")) +
            theme_bw(base_size = 14) +
            ylab("per 1,000") +
            scale_fill_manual(name = "", values = c("estimate" = "red")) +
            scale_color_manual(name = "", values = c("estimate" = "red", "observed" = "black"))

          fig <- ggplotly(p, tooltip = "text") # %>% layout(legend = list(x = 0.1, y = 0.9))
          fig[["x"]][["data"]][[1]][["name"]] <- "estimate"
          fig[["x"]][["data"]][[2]][["name"]] <- "observed"

          fig
        }
        else {
          p <- ggplot(
            data = d_res %>% filter(state == input$state, indicator == input$indicator, race == input$race, ci_width == 0.75),
            aes(year, observed_pop)
          ) +
            geom_line(aes(year, fit_pop, color = "estimate")) +
            geom_point(aes(color = "observed")) +
            geom_point(aes(year, fit_pop, text = sprintf(
              "%s<br> %s (%s, %s)",
              year, round(fit_pop),
              round(fit_lower_pop), round(fit_upper_pop)
            )), color = NA) +
            geom_ribbon(aes(ymin = fit_lower_pop, ymax = fit_upper_pop, fill = "estimate"), alpha = 0.2) +
            ggtitle(paste0(input$state, " (", state_div %>% filter(state == input$state) %>% select(division) %>% pull(), " division), ", input$race, " population")) +
            theme_bw(base_size = 14) +
            ylab("number") +
            scale_fill_manual(name = "", values = c("estimate" = "red")) +
            scale_color_manual(name = "", values = c("estimate" = "red", "observed" = "black"))

          fig <- ggplotly(p, tooltip = "text") # %>% layout(legend = list(x = 0.1, y = 0.9))
          fig[["x"]][["data"]][[1]][["name"]] <- "estimate"
          fig[["x"]][["data"]][[2]][["name"]] <- "observed"

          fig
        }
      })

      output$ProbTable <- renderTable({
        pr_res %>%
          filter(state == input$state, indicator == input$indicator, race == input$race, year > 2018) %>%
          select(-state, -indicator, -race) %>%
          mutate(year = as.character(year)) %>%
          rename(
            "Probability of increase from previous year" = pr_increase,
            "Probability of increase from 2018" = pr_increase_2018,
            Year = year
          )
      })
      output$CoefTable <- renderTable({
        betas %>%
          filter(indicator == input$indicator, race == input$race, division == state_div %>%
            filter(state == input$state) %>%
            select(division) %>%
            pull()) %>%
          filter(year == 2018) %>%
          arrange(-abs(value)) %>%
          mutate(
            sig = (lower < 0 & upper < 0) | lower > 0 & upper > 0,
            CI = paste0(" (", round(lower, 3), ",", round(upper, 3), ")"),
            estimate = round(value, 3)
          ) %>%
          select(Covariate = variable_description, Estimate = estimate, `80% CI` = CI) %>%
          head(5)
      })
    }
  )
}
