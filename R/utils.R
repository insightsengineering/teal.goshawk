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

#' Template function to generate reporter card for `teal.goshawk`
#' @param title (`character(1)`) title of the card (unless overwritten by label)
#' @param label (`character(1)`) label provided by the user when adding the card
#' @param description (`character(1)`) optional additional description
#' @param with_filter (`logical(1)`) flag indicating to add filter state
#' @param filter_panel_api (`FilterPanelAPI`) object with API that allows the generation
#' of the filter state in the report
#'
#' @return (`TealReportCard`) populated with a title, description and filter state
#'
#' @keywords internal
card_template <- function(title, label, description = NULL, with_filter, filter_panel_api) {
  card <- teal::TealReportCard$new()
  title <- if (label == "") title else label
  card$set_name(title)
  card$append_text(title, "header2")
  if (!is.null(description)) {
    card$append_text(description, "header3")
  }
  if (with_filter) {
    card$append_fs(filter_panel_api$get_filter_state())
  }
  card
}