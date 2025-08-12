#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tm_g_gh_scatterplot` is deprecated. 
#' Please use [tm_g_gh_correlationplot] instead.
#'
#' @param ... function is deprecated.
#'
#' @export
tm_g_gh_scatterplot <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.1.15",
    what = "tm_g_gh_scatterplot()",
    details = "You should use teal.goshawk::tm_g_gh_correlationplot instead of teal.goshawk::tm_g_gh_scatterplot"
  )
}
