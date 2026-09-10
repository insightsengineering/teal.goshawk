# Scatter Plot Teal Module For Biomarker Analysis

Scatter Plot Teal Module For Biomarker Analysis

## Usage

``` r
tm_g_gh_correlationplot(
  label,
  dataname = "ADLB",
  param_var = lifecycle::deprecated(),
  xaxis_param = teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"),
    teal.picks::values(selected = "ALT", multiple = FALSE), check_dataset = FALSE),
  xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
  yaxis_param = teal.picks::picks(teal.picks::variables("PARAMCD", "PARAMCD"),
    teal.picks::values(selected = "CRP", multiple = FALSE), check_dataset = FALSE),
  yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
  trt_group = teal.picks::variables(dplyr::starts_with("ARM"), selected = "ARM"),
  color_manual = NULL,
  shape_manual = NULL,
  facet_ncol = 2,
  visit_facet = TRUE,
  trt_facet = FALSE,
  reg_line = FALSE,
  loq_legend = TRUE,
  rotate_xlab = FALSE,
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  hline_vars = character(0),
  hline_vars_colors = "green",
  hline_vars_labels = hline_vars,
  vline_arb = numeric(0),
  vline_arb_color = "red",
  vline_arb_label = "Vertical line",
  vline_vars = character(0),
  vline_vars_colors = "green",
  vline_vars_labels = vline_vars,
  plot_height = c(500, 200, 2000),
  plot_width = NULL,
  font_size = c(12, 8, 20),
  dot_size = c(1, 1, 12),
  reg_text_size = c(3, 3, 10),
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

- xaxis_param:

  ([`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  biomarker selected for `x-axis`.

- xaxis_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  name of variable containing biomarker results displayed on x-axis e.g.
  `BASE`.

- yaxis_param:

  ([`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  biomarker selected for `y-axis`.

- yaxis_var:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  name of variable containing biomarker results displayed on y-axis e.g.
  `AVAL`.

- trt_group:

  ([`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  or legacy
  [`teal.transform::choices_selected()`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html))
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- color_manual:

  (named `character`, optional) vector of colors applied to treatment
  values.

- shape_manual:

  (named `numeric`, optional) vector of symbols applied to `LOQ` values.

- facet_ncol:

  (`integer(1)`) numeric value indicating number of facets per row.

- visit_facet:

  (`logical(1)`) visit facet toggle.

- trt_facet:

  (`logical(1)`) facet by treatment group `trt_group`.

- reg_line:

  (`logical(1)`) include regression line and annotations for slope and
  coefficient in visualization. Use with facet `TRUE`.

- loq_legend:

  (`logical(1)`) `loq` legend toggle.

- rotate_xlab:

  (`logical(1)`) 45 degree rotation of `x-axis` values.

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

- vline_arb:

  (`numeric`) vector of at most 2 values identifying intercepts for
  arbitrary vertical lines.

- vline_arb_color:

  (`character`) a character vector of at most length of `vline_arb`.
  naming the color for the arbitrary vertical lines.

- vline_arb_label:

  (`character`) a character vector of at most length of `vline_arb`.
  naming the label for the arbitrary vertical lines.

- vline_vars:

  (`character`) a character vector to name the columns that will define
  additional vertical lines.

- vline_vars_colors:

  (`character`) a character vector naming the colors for the additional
  vertical lines.

- vline_vars_labels:

  (`character`) a character vector naming the labels for the additional
  vertical lines that will appear in the plot.

- plot_height:

  (`numeric(3)`) controls plot height.

- plot_width:

  (`numeric(3)`, optional) controls plot width.

- font_size:

  (`numeric(3)`) font size control for title, `x-axis` label, `y-axis`
  label and legend.

- dot_size:

  (`numeric(3)`) plot dot size.

- reg_text_size:

  (`numeric(3)`) font size control for regression line annotations.

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

    tm_g_gh_correlationplot(
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

  # use non-exported function from goshawk
  .h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")

  # original ARM value = dose value
  .arm_mapping <- list(
    "A: Drug X" = "150mg QD",
    "B: Placebo" = "Placebo",
    "C: Combination" = "Combination"
  )
  .color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
  # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
  .shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)

  ADSL <- teal.data::rADSL
  ADLB <- teal.data::rADLB
  .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
  ADLB <- ADLB %>%
    mutate(AVISITCD = case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~
        paste(
          "W",
          trimws(
            substr(
              AVISIT,
              start = 6,
              stop = str_locate(AVISIT, "DAY") - 1
            )
          )
        ),
      TRUE ~ NA_character_
    )) %>%
    mutate(AVISITCDN = case_when(
      AVISITCD == "SCR" ~ -2,
      AVISITCD == "BL" ~ 0,
      grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
      TRUE ~ NA_real_
    )) %>%
    # use ARMCD values to order treatment in visualization legend
    mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
      ifelse(grepl("B", ARMCD), 2,
        ifelse(grepl("A", ARMCD), 3, NA)
      )
    )) %>%
    mutate(ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))])) %>%
    mutate(ARM = factor(ARM) %>%
      reorder(TRTORD)) %>%
    mutate(
      ANRHI = case_when(
        PARAMCD == "ALT" ~ 60,
        PARAMCD == "CRP" ~ 70,
        PARAMCD == "IGA" ~ 80,
        TRUE ~ NA_real_
      ),
      ANRLO = case_when(
        PARAMCD == "ALT" ~ 20,
        PARAMCD == "CRP" ~ 30,
        PARAMCD == "IGA" ~ 40,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    group_by(PARAMCD) %>%
    mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
    )) %>%
    mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
    )) %>%
    ungroup()
  attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
  attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
  attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"

  # add LLOQ and ULOQ variables
  ADLB_LOQS <- .h_identify_loq_values(ADLB, "LOQFL")
  ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
})

join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_gh_correlationplot(
      label = "Correlation Plot",
      dataname = "ADLB",
      xaxis_param = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      yaxis_param = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "CRP", multiple = FALSE),
        check_dataset = FALSE
      ),
      xaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
      yaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = variables(c("ARM", "ACTARM"), "ARM"),
      color_manual = c(
        "Drug X 100mg" = "#000000",
        "Placebo" = "#3498DB",
        "Combination 100mg" = "#E74C3C"
      ),
      shape_manual = c("N" = 1, "Y" = 2, "NA" = 0),
      plot_height = c(500, 200, 2000),
      facet_ncol = 2,
      visit_facet = TRUE,
      reg_line = FALSE,
      loq_legend = TRUE,
      font_size = c(12, 8, 20),
      dot_size = c(1, 1, 12),
      reg_text_size = c(3, 3, 10),
      hline_arb = c(40, 50),
      hline_arb_label = "arb hori label",
      hline_arb_color = c("red", "blue"),
      hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      hline_vars_colors = c("green", "blue", "purple", "cyan"),
      hline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
      vline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      vline_vars_colors = c("yellow", "orange", "brown", "gold"),
      vline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
      vline_arb = c(50, 70),
      vline_arb_label = "arb vert A",
      vline_arb_color = c("green", "orange")
    )
  )
)
#> Initializing tm_g_gh_correlationplot
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
