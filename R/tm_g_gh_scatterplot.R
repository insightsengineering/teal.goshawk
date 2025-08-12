#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#'  `tm_g_gh_scatterplot` is deprecated. Please use [tm_g_gh_correlationplot]
#'   instead.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#' laboratory data frame \code{ADLB}.
#' @param param_var name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on `x-axis` e.g. \code{BASE}.
#' @param yaxis_var name of variable containing biomarker results displayed on `y-axis` e.g. \code{AVAL}.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. `ARM`.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param trt_facet facet by treatment group \code{trt_group}.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param rotate_xlab 45 degree rotation of `x-axis` values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, `x-axis` label, `y-axis` label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#'
#' @export
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#'
tm_g_gh_scatterplot <- function(label,
                                dataname,
                                param_var,
                                param,
                                xaxis_var,
                                yaxis_var,
                                trt_group,
                                color_manual = NULL,
                                shape_manual = NULL,
                                facet_ncol = 2,
                                trt_facet = FALSE,
                                reg_line = FALSE,
                                rotate_xlab = FALSE,
                                hline = NULL,
                                vline = NULL,
                                plot_height = c(500, 200, 2000),
                                plot_width = NULL,
                                font_size = c(12, 8, 20),
                                dot_size = c(1, 1, 12),
                                reg_text_size = c(3, 3, 10),
                                pre_output = NULL,
                                post_output = NULL,
                                transformators = list()) {
  lifecycle::deprecate_stop(
    when = "0.1.15",
    what = "tm_g_gh_scatterplot()",
    details = "You should use teal.goshawk::tm_g_gh_correlationplot instead of teal.goshawk::tm_g_gh_scatterplot"
  )
}
