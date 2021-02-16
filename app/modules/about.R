about_ui <- function() {
  tabPanel(
    "About",
    div(
      class = "app-page",
      h2("About this project"),
      fluidRow(
        class = "app-row",
          HTML(
            "This app shows the preliminary results of a projection model of foster care outcomes by state in the United States.",
            "<br/>", "<br/>", "The projections are based on a Bayesian hirerchical state space model. In brief, foster care entries are modeled as a function of a set of demographic, socioeconomic, health and welfare variables (shown in the 'Covariates' tab). The association between each of the variables is allowed to vary across geography (census division) and time.",
            "<br/>", "<br/>", "Code can be found at: https://github.com/MJAlexander/child_welfare_projections."
          )
        )
      )
  )
}
