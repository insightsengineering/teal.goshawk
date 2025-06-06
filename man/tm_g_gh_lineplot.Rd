% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_g_gh_lineplot.R
\name{tm_g_gh_lineplot}
\alias{tm_g_gh_lineplot}
\title{Line plot}
\usage{
tm_g_gh_lineplot(
  label,
  dataname,
  param_var,
  param,
  param_var_label = "PARAM",
  xaxis_var,
  yaxis_var,
  xvar_level = NULL,
  filter_var = yaxis_var,
  filter_var_choices = filter_var,
  trt_group,
  trt_group_level = NULL,
  shape_choices = NULL,
  stat = "mean",
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  color_manual = c(getOption("ggplot2.discrete.colour"), c("#ff0000", "#008000",
    "#4ca3dd", "#8a2be2"))[1:4],
  xtick = ggplot2::waiver(),
  xlabel = xtick,
  rotate_xlab = FALSE,
  plot_height = c(600, 200, 4000),
  plot_width = NULL,
  plot_font_size = c(12, 8, 20),
  dodge = c(0.4, 0, 1),
  pre_output = NULL,
  post_output = NULL,
  count_threshold = 0,
  table_font_size = c(12, 4, 20),
  dot_size = c(2, 1, 12),
  plot_relative_height_value = 1000,
  transformators = list()
)
}
\arguments{
\item{label}{menu item label of the module in the teal app.}

\item{dataname}{analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. \code{ADaM} structured
laboratory data frame \code{ADLB}.}

\item{param_var}{name of variable containing biomarker codes e.g. \code{PARAMCD}.}

\item{param}{biomarker selected.}

\item{param_var_label}{single name of variable in analysis data that includes parameter labels.}

\item{xaxis_var}{single name of variable in analysis data that is used as x-axis in the plot for the
respective \code{goshawk} function.}

\item{yaxis_var}{single name of variable in analysis data that is used as summary variable in the
respective \code{goshawk} function.}

\item{xvar_level}{vector that can be used to define the factor level of \code{xvar}. Only use it when
\code{xvar} is character or factor.}

\item{filter_var}{data constraint variable.}

\item{filter_var_choices}{data constraint variable choices.}

\item{trt_group}{\code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
for variable names representing treatment group e.g. \code{ARM}.}

\item{trt_group_level}{vector that can be used to define factor level of \code{trt_group}.}

\item{shape_choices}{Vector or \code{choices_selected} object with names of \code{ADSL} variables which
can be used to change shape}

\item{stat}{string of statistics}

\item{hline_arb}{numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.}

\item{hline_arb_color}{a character vector of at most length of \code{hline_arb}.
naming the color for the arbitrary horizontal lines.}

\item{hline_arb_label}{a character vector of at most length of \code{hline_arb}.
naming the label for the arbitrary horizontal lines.}

\item{color_manual}{string vector representing customized colors}

\item{xtick}{numeric vector to define the tick values of x-axis when x variable is numeric.
Default value is waive().}

\item{xlabel}{vector with same length of \code{xtick} to define the label of x-axis tick values.
Default value is waive().}

\item{rotate_xlab}{\code{logical(1)} value indicating whether to rotate \code{x-axis} labels.}

\item{plot_height}{controls plot height.}

\item{plot_width}{optional, controls plot width.}

\item{plot_font_size}{control font size for title, \code{x-axis}, \code{y-axis} and legend font.}

\item{dodge}{controls the position dodge of error bar}

\item{pre_output}{(\code{shiny.tag}) optional,\cr
with text placed before the output to put the output into context. For example a title.}

\item{post_output}{(\code{shiny.tag}) optional, with text placed after the output to put the output
into context. For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

\item{count_threshold}{minimum count of observations (as listed in the output table) to plot
nodes on the graph}

\item{table_font_size}{controls the font size of values in the table}

\item{dot_size}{plot dot size.}

\item{plot_relative_height_value}{numeric value between 500 and 5000 for controlling the starting value
of the relative plot height slider}

\item{transformators}{(\code{list} of \code{teal_transform_module}) that will be applied to transform module's data input.
To learn more check \code{vignette("transform-input-data", package = "teal")}.}
}
\value{
\code{shiny} object
}
\description{
This teal module renders the UI and calls the function that creates a line plot.
}
\examples{
\dontshow{if (require("nestcolor")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Example using ADaM structure analysis dataset.
data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(stringr)
  library(nestcolor)

  # original ARM value = dose value
  .arm_mapping <- list(
    "A: Drug X" = "150mg QD",
    "B: Placebo" = "Placebo",
    "C: Combination" = "Combination"
  )

  ADSL <- rADSL
  ADLB <- rADLB
  .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
  ADLB <- ADLB \%>\%
    mutate(
      AVISITCD = case_when(
        AVISIT == "SCREENING" ~ "SCR",
        AVISIT == "BASELINE" ~ "BL",
        grepl("WEEK", AVISIT) ~ paste("W", str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
        TRUE ~ as.character(NA)
      ),
      AVISITCDN = case_when(
        AVISITCD == "SCR" ~ -2,
        AVISITCD == "BL" ~ 0,
        grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
        TRUE ~ as.numeric(NA)
      ),
      AVISITCD = factor(AVISITCD) \%>\% reorder(AVISITCDN),
      TRTORD = case_when(
        ARMCD == "ARM C" ~ 1,
        ARMCD == "ARM B" ~ 2,
        ARMCD == "ARM A" ~ 3
      ),
      ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))]),
      ARM = factor(ARM) \%>\% reorder(TRTORD),
      ACTARM = as.character(.arm_mapping[match(ACTARM, names(.arm_mapping))]),
      ACTARM = factor(ACTARM) \%>\% reorder(TRTORD)
    )
  attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
  attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_gh_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
      shape_choices = c("SEX", "RACE"),
      xaxis_var = choices_selected("AVISITCD", "AVISITCD"),
      yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
      hline_arb = c(20.5, 19.5),
      hline_arb_color = c("red", "green"),
      hline_arb_label = c("A", "B")
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
\dontshow{\}) # examplesIf}
}
\author{
Wenyi Liu (luiw2) wenyi.liu@roche.com

Balazs Toth (tothb2) toth.balazs@gene.com
}
