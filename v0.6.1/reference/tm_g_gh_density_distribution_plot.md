# Density Distribution Plot

This teal module renders the UI and calls the functions that create a
density distribution plot and an accompanying summary table.

## Usage

``` r
tm_g_gh_density_distribution_plot(
  label,
  dataname = "ADLB",
  param_var = lifecycle::deprecated(),
  param = teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"),
    teal.picks::values(selected = "ALT", multiple = FALSE), check_dataset = FALSE),
  xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
  trt_group = teal.picks::variables(dplyr::starts_with("ARM"), selected = "ARM"),
  color_manual = NULL,
  color_comb = NULL,
  plot_height = c(500, 200, 2000),
  plot_width = NULL,
  font_size = c(12, 8, 20),
  line_size = c(1, 0.25, 3),
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  facet_ncol = 2L,
  comb_line = TRUE,
  rotate_xlab = FALSE,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) menu item label of the module in the teal app.

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

- xaxis_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  name of variable containing biomarker results displayed on x-axis e.g.
  `BASE`.

- trt_group:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- color_manual:

  (named `character`, optional) vector of colors applied to treatment
  values.

- color_comb:

  name or hex value for combined treatment color.

- plot_height:

  (`numeric(3)`) controls plot height.

- plot_width:

  (`numeric(3)`, optional) controls plot width.

- font_size:

  (`numeric(3)`) font size control for title, `x-axis` label, `y-axis`
  label and legend.

- line_size:

  plot line thickness.

- hline_arb:

  (`numeric`) vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the color for the arbitrary horizontal lines.

- hline_arb_label:

  (`character`) a character vector of at most length of `hline_arb`.
  naming the label for the arbitrary horizontal lines.

- facet_ncol:

  (`integer(1)`) numeric value indicating number of facets per row.

- comb_line:

  display combined treatment line toggle.

- rotate_xlab:

  (`logical(1)`) 45 degree rotation of `x-axis` values.

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

    tm_g_gh_density_distribution_plot(
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

Nick Paszty

Balazs Toth

## Examples

``` r
# Example using ADaM structure analysis dataset.
data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(stringr)

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
    tm_g_gh_density_distribution_plot(
      label = "Density Distribution Plot",
      dataname = "ADLB",
      param = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      xaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = variables(c("ARM", "ACTARM"), "ARM"),
      color_manual = c(
        "150mg QD" = "#000000",
        "Placebo" = "#3498DB",
        "Combination" = "#E74C3C"
      ),
      color_comb = "#39ff14",
      comb_line = TRUE,
      plot_height = c(500, 200, 2000),
      font_size = c(12, 8, 20),
      line_size = c(1, .25, 3),
      hline_arb = c(.02, .05),
      hline_arb_color = c("red", "black"),
      hline_arb_label = c("Horizontal Line A", "Horizontal Line B")
    )
  )
)
#> Initializing tm_g_gh_density_distribution_plot
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
