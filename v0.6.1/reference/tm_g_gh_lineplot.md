# Line plot

This teal module renders the UI and calls the function that creates a
line plot.

## Usage

``` r
tm_g_gh_lineplot(
  label,
  dataname = "ADLB",
  param_var = lifecycle::deprecated(),
  param = teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"),
    teal.picks::values(selected = "ALT", multiple = FALSE), check_dataset = FALSE),
  param_var_label = "PARAM",
  xaxis_var = teal.picks::variables(dplyr::starts_with("AVISIT"), "AVISITCD"),
  yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
  xvar_level = NULL,
  filter_var = lifecycle::deprecated(),
  filter_var_choices = lifecycle::deprecated(),
  trt_group = teal.picks::variables(selected = "ARM"),
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
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- dataname:

  (`character(1)`) analysis data passed to the data argument of
  [`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  E.g. `ADaM` structured laboratory data frame `ADLB`.

- param_var:

  **\[deprecated\]** (`character(1)`) name of variable containing
  biomarker codes e.g. `PARAMCD`.

- param:

  ([`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  biomarker selected.

- param_var_label:

  (`character(1)`) single name of variable in analysis data that
  includes parameter labels.

- xaxis_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  name of variable containing biomarker results displayed on x-axis e.g.
  `BASE`.

- yaxis_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  name of variable containing biomarker results displayed on y-axis e.g.
  `AVAL`.

- xvar_level:

  ([`character()`](https://rdrr.io/r/base/character.html)) vector that
  can be used to define the factor level of `xvar`. Only use it when
  `xaxis_var` is of type character or factor.

- filter_var:

  **\[deprecated\]** data constraint variable.

- filter_var_choices:

  **\[deprecated\]** data constraint variable choices.

- trt_group:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- trt_group_level:

  (`named character()`) vector that can be used to define factor level
  of `trt_group`.

- shape_choices:

  ([`character()`](https://rdrr.io/r/base/character.html),
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  vector or
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  object with names of `ADSL` variables which can be used to change
  shape

- stat:

  (`character(1)`) string of statistics

- hline_arb:

  (`numeric`) vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the color for the arbitrary horizontal lines.

- hline_arb_label:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the label for the arbitrary horizontal lines.

- color_manual:

  (named `character`, optional) vector of colors applied to treatment
  values.

- xtick:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) numeric vector to
  define the tick values of x-axis when x variable is numeric. Default
  value is waive().

- xlabel:

  ([`character()`](https://rdrr.io/r/base/character.html)) vector with
  same length of `xtick` to define the label of x-axis tick values.
  Default value is waive().

- rotate_xlab:

  (`logical(1)`) 45 degree rotation of `x-axis` values.

- plot_height:

  (`numeric(3)`) controls plot height.

- plot_width:

  (`numeric(3)`, optional) controls plot width.

- plot_font_size:

  (`numeric(3)`) control font size for title, `x-axis`, `y-axis` and
  legend font.

- dodge:

  (`numeric(3)`) controls the position dodge of error bar

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional, with text placed after the output to put the
  output into context. For example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- count_threshold:

  (`numeric(1)`) minimum count of observations (as listed in the output
  table) to plot nodes on the graph

- table_font_size:

  (`numeric(3)`) controls the font size of values in the table.

- dot_size:

  (`numeric(3)`) plot dot size.

- plot_relative_height_value:

  (`numeric(1)`) numeric value between 500 and 5000 for controlling the
  starting value of the relative plot height slider

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

  See section "Decorating Module" below for more details.

## Value

A
[`teal::module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
object that can be used in a
[`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
call.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_gh_lineplot(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...) # applied only to `plot` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.goshawk")`](https://insightsengineering.github.io/teal.goshawk/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

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

Wenyi Liu

Balazs Toth

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
  ADSL <- teal.data::rADSL
  ADLB <- teal.data::rADLB
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
      param = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      shape_choices = c("SEX", "RACE"),
      xaxis_var = variables("AVISITCD", "AVISITCD"),
      yaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = variables(c("ARM", "ACTARM"), "ARM"),
      hline_arb = c(20.5, 19.5),
      hline_arb_color = c("red", "green"),
      hline_arb_label = c("A", "B")
    )
  )
)
#> Initializing tm_g_gh_lineplot
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
