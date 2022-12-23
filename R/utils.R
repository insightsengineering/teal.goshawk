#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method.
#'
#' @param pattern (`character`) pattern of files to be included
#'
#' @return HTML code that includes `CSS` files
#' @keywords internal
include_css_files <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "teal.goshawk", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  if (length(css_files) == 0) {
    return(NULL)
  }
  return(shiny::singleton(shiny::tags$head(lapply(css_files, shiny::includeCSS))))
}

plots_per_row_validate_rules <- function(required = TRUE) {
  msg <- "Number of plots per row must be a positive integer"
  shinyvalidate::compose_rules(
    if (required) {
      shinyvalidate::sv_required(msg)
    } else {
      shinyvalidate::sv_optional()
    },
    shinyvalidate::sv_integer(msg),
    shinyvalidate::sv_gt(0, message_fmt = msg)
  )
}