about_ui <- function() {
  tabPanel(
    "About",
    div(
      class = "app-page",
      h2("About this project"),
      fluidRow(
        class = "app-row",
        includeMarkdown("www/about.md")
        )
      )
  )
}
