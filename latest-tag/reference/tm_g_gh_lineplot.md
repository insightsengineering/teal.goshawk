# Line plot

This teal module renders the UI and calls the function that creates a
line plot.

## Usage

``` r
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
```

## Arguments

- label:

  menu item label of the module in the teal app.

- dataname:

  analysis data passed to the data argument of
  [`init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  E.g. `ADaM` structured laboratory data frame `ADLB`.

- param_var:

  name of variable containing biomarker codes e.g. `PARAMCD`.

- param:

  biomarker selected.

- param_var_label:

  single name of variable in analysis data that includes parameter
  labels.

- xaxis_var:

  single name of variable in analysis data that is used as x-axis in the
  plot for the respective `goshawk` function.

- yaxis_var:

  single name of variable in analysis data that is used as summary
  variable in the respective `goshawk` function.

- xvar_level:

  vector that can be used to define the factor level of `xvar`. Only use
  it when `xvar` is character or factor.

- filter_var:

  data constraint variable.

- filter_var_choices:

  data constraint variable choices.

- trt_group:

  [`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- trt_group_level:

  vector that can be used to define factor level of `trt_group`.

- shape_choices:

  Vector or `choices_selected` object with names of `ADSL` variables
  which can be used to change shape

- stat:

  string of statistics

- hline_arb:

  numeric vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  a character vector of at most length of `hline_arb`. naming the color
  for the arbitrary horizontal lines.

- hline_arb_label:

  a character vector of at most length of `hline_arb`. naming the label
  for the arbitrary horizontal lines.

- color_manual:

  string vector representing customized colors

- xtick:

  numeric vector to define the tick values of x-axis when x variable is
  numeric. Default value is waive().

- xlabel:

  vector with same length of `xtick` to define the label of x-axis tick
  values. Default value is waive().

- rotate_xlab:

  `logical(1)` value indicating whether to rotate `x-axis` labels.

- plot_height:

  controls plot height.

- plot_width:

  optional, controls plot width.

- plot_font_size:

  control font size for title, `x-axis`, `y-axis` and legend font.

- dodge:

  controls the position dodge of error bar

- pre_output:

  (`shiny.tag`) optional,  
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional, with text placed after the output to put the
  output into context. For example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- count_threshold:

  minimum count of observations (as listed in the output table) to plot
  nodes on the graph

- table_font_size:

  controls the font size of values in the table

- dot_size:

  plot dot size.

- plot_relative_height_value:

  numeric value between 500 and 5000 for controlling the starting value
  of the relative plot height slider

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

## Value

`shiny` object

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Author

Wenyi Liu (luiw2) wenyi.liu@roche.com

Balazs Toth (tothb2) toth.balazs@gene.com

## Examples

``` r
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
  set.seed(1) # @linksto ADSL ADLB
  ADSL <- rADSL
  ADLB <- rADLB
  .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
  ADLB <- ADLB %>%
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
      AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
      TRTORD = case_when(
        ARMCD == "ARM C" ~ 1,
        ARMCD == "ARM B" ~ 2,
        ARMCD == "ARM A" ~ 3
      ),
      ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))]),
      ARM = factor(ARM) %>% reorder(TRTORD),
      ACTARM = as.character(.arm_mapping[match(ACTARM, names(.arm_mapping))]),
      ACTARM = factor(ACTARM) %>% reorder(TRTORD)
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
#> Initializing tm_g_gh_lineplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
