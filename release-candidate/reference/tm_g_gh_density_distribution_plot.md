# Density Distribution Plot

This teal module renders the UI and calls the functions that create a
density distribution plot and an accompanying summary table.

## Usage

``` r
tm_g_gh_density_distribution_plot(
  label,
  dataname,
  param_var,
  param,
  xaxis_var,
  trt_group,
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
  transformators = list()
)
```

## Arguments

- label:

  menu item label of the module in the teal app.

- dataname:

  analysis data passed to the data argument of
  [`init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html).
  E.g. `ADaM` structured

- param_var:

  name of variable containing biomarker codes e.g. `PARAMCD`.

- param:

  biomarker selected.

- xaxis_var:

  name of variable containing biomarker results displayed on `x-axis`
  e.g. `BASE`.

- trt_group:

  [`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- color_manual:

  vector of colors applied to treatment values.

- color_comb:

  name or hex value for combined treatment color.

- plot_height:

  controls plot height.

- plot_width:

  optional, controls plot width.

- font_size:

  font size control for title, `x-axis` label, `y-axis` label and
  legend.

- line_size:

  plot line thickness.

- hline_arb:

  numeric vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  a character vector of at most length of `hline_arb`. naming the color
  for the arbitrary horizontal lines.

- hline_arb_label:

  a character vector of at most length of `hline_arb`. naming the label
  for the arbitrary horizontal lines.

- facet_ncol:

  numeric value indicating number of facets per row.

- comb_line:

  display combined treatment line toggle.

- rotate_xlab:

  45 degree rotation of `x-axis` values.

- pre_output:

  (`shiny.tag`) optional,  
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

## Details

None

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

Nick Paszty (npaszty) paszty.nicholas@gene.com

Balazs Toth (tothb2) toth.balazs@gene.com

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
    tm_g_gh_density_distribution_plot(
      label = "Density Distribution Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
      xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
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
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
