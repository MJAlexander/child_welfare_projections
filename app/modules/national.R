national_ui <- function(id, d_res) {
  ns <- shiny::NS(id)

  tabPanel(
    "National",
    div(
      class = "app-page",
      h2("National overview"),

      fluidRow(
        class = "app-row",
        "Select a year and indicator below to to see trends for the United States. Mouse over map to see estimate with 80% credible intervals."
      ),
      hr(),
      fluidRow(
        class = "app-row",
        column(
          width = 3,
          sliderInput(
            inputId = ns("year"), label = "Year",
            min = 2005, # min(d_res$year),
            max = max(d_res$year),
            value = min(d_res$year),
            sep = "",
            animate = animationOptions(interval = 1000)
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
      # Show a plot of the generated distribution
      column(12, align = "center",
      withSpinner(plotlyOutput(ns("MapPlot"), width = "80%", height = "500px"))
      )
    )
  )
}

national_server <- function(id, d_res, merged_data, Noax) {
  moduleServer(
    id,
    function(input, output, session) {

      # National Map

      output$MapPlot <- renderPlotly({
        if (input$type == "per capita") {
          p <- ggplot(
            data = merged_data %>% filter(
              year == input$year,
              indicator == input$indicator,
              race == input$race,
              ci_width == 0.75
            ),
            aes(text = sprintf("%s<br> %s (%s, %s)", state, round(fit * 1000, 2), round(fit_lower * 1000, 2), round(fit_upper * 1000, 2)))
          ) +
            geom_polygon(aes(x = long, y = lat, group = group, fill = fit * 1000), color = "black") +
            scale_fill_viridis_c(
              name = paste0(input$indicator, " per 1,000"),
              limits = c(
                min(d_res$fit[d_res$indicator == input$indicator & d_res$race == input$race] * 1000) * 0.95,
                max(d_res$fit[d_res$indicator == input$indicator & d_res$race == input$race] * 1000) * 1
              )
            ) +
            theme_void() +
            coord_fixed(1.5) +
            ggtitle(paste0("Foster care ", str_to_lower(input$indicator), ", ", input$year, " (per 1000)\n", input$race, " population"))
          fig <- ggplotly(p, tooltip = "text") %>%
            layout(xaxis = Noax, yaxis = Noax) %>%
            config(displayModeBar = FALSE)

          fig
        }
        else {
          p <- ggplot(
            data = merged_data %>% filter(
              year == input$year,
              indicator == input$indicator,
              race == input$race,
              ci_width == 0.75
            ),
            aes(text = sprintf("%s<br> %s (%s, %s)", state, round(fit_pop), round(fit_lower_pop), round(fit_upper_pop)))
          ) +
            geom_polygon(aes(x = long, y = lat, group = group, fill = fit_pop), color = "black") +
            scale_fill_viridis_c(
              name = "number",
              limits = c(
                min(d_res$fit_pop[d_res$indicator == input$indicator]) * 0.95,
                max(d_res$fit_pop[d_res$indicator == input$indicator]) * 1
              )
            ) +
            theme_void() +
            coord_fixed(1.5) +
            ggtitle(paste0("Foster care ", str_to_lower(input$indicator), ", ", input$year, " (number)\n", input$race, " population"))
          fig <- ggplotly(p, tooltip = "text") %>%
            layout(xaxis = Noax, yaxis = Noax)

          fig
        }
      })
    }
  )
}
