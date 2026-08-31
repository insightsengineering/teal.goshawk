# Spaghetti Plot

This teal module renders the UI and calls the function that creates a
spaghetti plot.

## Usage

``` r
tm_g_gh_spaghettiplot(
  label,
  dataname = "ADLB",
  param_var = lifecycle::deprecated(),
  param = teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"),
    teal.picks::values(selected = "ALT", multiple = FALSE), check_dataset = FALSE),
  param_var_label = "PARAM",
  idvar = "USUBJID",
  xaxis_var = teal.picks::variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
  yaxis_var = teal.picks::variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
  xaxis_var_level = NULL,
  filter_var = lifecycle::deprecated(),
  trt_group = teal.picks::variables(dplyr::starts_with("ARM"), selected = "ARM"),
  trt_group_level = NULL,
  group_stats = "NONE",
  man_color = NULL,
  color_comb = NULL,
  xtick = ggplot2::waiver(),
  xlabel = xtick,
  rotate_xlab = FALSE,
  facet_ncol = 2,
  free_x = FALSE,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  font_size = c(12, 8, 20),
  dot_size = c(2, 1, 12),
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  hline_vars = character(0),
  hline_vars_colors = "green",
  hline_vars_labels = hline_vars,
  alpha = c(0.8, 0, 1),
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
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

- idvar:

  name of unique subject id variable.

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

- xaxis_var_level:

  vector that can be used to define the factor level of `xaxis_var`.
  Only use it when `xaxis_var` is character or factor.

- filter_var:

  **\[deprecated\]** data constraint variable.

- trt_group:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- trt_group_level:

  (`named character()`) vector that can be used to define factor level
  of `trt_group`.

- group_stats:

  control group mean or median overlay.

- man_color:

  string vector representing customized colors

- color_comb:

  name or hex value for combined treatment color.

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

- facet_ncol:

  (`integer(1)`) numeric value indicating number of facets per row.

- free_x:

  `logical(1)` should scales be `"fixed"` (`FALSE`) of `"free"` (`TRUE`)
  for `x-axis` in
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  `scales` parameter.

- plot_height:

  (`numeric(3)`) controls plot height.

- plot_width:

  (`numeric(3)`, optional) controls plot width.

- font_size:

  (`numeric(3)`) font size control for title, `x-axis` label, `y-axis`
  label and legend.

- dot_size:

  (`numeric(3)`) plot dot size.

- hline_arb:

  (`numeric`) vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the color for the arbitrary horizontal lines.

- hline_arb_label:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the label for the arbitrary horizontal lines.

- hline_vars:

  (`character`) a character vector to name the columns that will define
  additional horizontal lines.

- hline_vars_colors:

  (`character`) a character vector naming the colors for the additional
  horizontal lines.

- hline_vars_labels:

  (`character`) a character vector naming the labels for the additional
  horizontal lines that will appear in the plot.

- alpha:

  (`numeric(3)`) vector to define transparency of plotted points.

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional, with text placed after the output to put the
  output into context. For example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

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

    tm_g_gh_spaghettiplot(
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

  # use non-exported function from goshawk
  .h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")

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
      ACTARM = factor(ACTARM) %>% reorder(TRTORD),
      ANRLO = 30,
      ANRHI = 75
    ) %>%
    rowwise() %>%
    group_by(PARAMCD) %>%
    mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
    )) %>%
    mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
    )) %>%
    ungroup()
  attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
  attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
  attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
  attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"

  # add LLOQ and ULOQ variables
  ALB_LOQS <- .h_identify_loq_values(ADLB, "LOQFL")
  ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_gh_spaghettiplot(
      label = "Spaghetti Plot",
      dataname = "ADLB",
      param = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      idvar = "USUBJID",
      xaxis_var = variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
      yaxis_var = variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
      trt_group = variables(c("ARM", "ACTARM"), "ARM"),
      color_comb = "#39ff14",
      man_color = c(
        "Combination" = "#000000",
        "Placebo" = "#fce300",
        "150mg QD" = "#5a2f5f"
      ),
      hline_arb = c(60, 50),
      hline_arb_color = c("grey", "red"),
      hline_arb_label = c("default A", "default B"),
      hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      hline_vars_colors = c("pink", "brown", "purple", "black")
    )
  )
)
#> Initializing tm_g_gh_spaghettiplot
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
