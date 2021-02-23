app_page <- function(title, header, theme, ...) {

  # Construct tabs (navbar and actual content) based on inputs
  tabs <- shiny:::buildTabset(tabs = list(...), ulClass = "nav navbar-nav")

  # Create div for navbar
  div_navbar_container <- shiny::div(class = "navbar-container app-page", tabs$navList)

  # Create div for content (actual pages)
  div_content <- shiny::div(class = "container-fluid", tabs$content)

  # Create page
  shiny::bootstrapPage(
    title = title,
    theme = theme,
    shiny::tags$nav(class = "navbar navbar-default", role = "navigation", div_navbar_container),
    header,
    shiny::hr(),
    div_content
  )
}
