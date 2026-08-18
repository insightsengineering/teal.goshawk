#' teal.goshawk core packages
#'
#' The teal.goshawk package renders the UI and calls the respective biomarker visualization functions.
#'
#' The data used for teal.goshawk have some constraints.
#' It must contain the columns `AVISITCD`, `BASE`, `BASE2`, `AVALU`, `LBSTRESC`, `LOQFL`, `CHG2`, and `PCHG2`.
#'
#' @import goshawk
#' @import shiny
#' @import teal
#' @import teal.transform
#' @import teal.picks
#' @importFrom ggplot2 ggplot
#' @importFrom rlang .data sym
#'
#' @keywords internal
"_PACKAGE"
