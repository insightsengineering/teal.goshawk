#' teal.goshawk core packages
#'
#' The teal.goshawk package renders the UI and calls the respective biomarker visualization functions.
#'
#' The data used for teal.goshawk have some constraints.
#' It must contain the columns `AVISITCD`, `BASE`, `BASE2`, `AVALU`, `LBSTRESC`, `LBSTRESC`.
#'
#' @docType package
#'
#' @name teal_goshawk
#'
#' @import goshawk
#' @import shiny
#' @import teal
#' @importFrom ggplot2 ggplot
#' @importFrom rlang .data sym
#' @importFrom teal.transform choices_selected
#' @keywords internal
NULL
