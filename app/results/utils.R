horizontal_inputs <- function(..., widths = NULL) {
  inputs <- list(...)

  if (is.null(widths)) {
    widths <- 12 / length(inputs)

    if (widths %% 1 != 0) {
      stop("Inputs cannot be evenly spaced into a grid of 12 - please supply widths in `widths`.", call. = FALSE)
    }

    widths <- rep(widths, length(inputs))
  } if (!is.null(widths) & sum(widths) > 12) {
    stop("`widths` must be 12 or less", call. = FALSE)
  }

  fluidRow(
    class = "app-row",
    purrr::map2(inputs, widths, ~ column(width = .y, .x))
  )
}
