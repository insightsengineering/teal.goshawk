# Box Plot

This teal module renders the UI and calls the functions that create a
box plot and accompanying summary table.

## Usage

``` r
tm_g_gh_boxplot(
  label,
  dataname,
  param_var,
  param,
  yaxis_var = teal.transform::choices_selected(c("AVAL", "CHG"), "AVAL"),
  xaxis_var = teal.transform::choices_selected("AVISITCD", "AVISITCD"),
  facet_var = teal.transform::choices_selected(c("ARM", "ACTARM"), "ARM"),
  trt_group,
  color_manual = NULL,
  shape_manual = NULL,
  facet_ncol = NULL,
  loq_legend = TRUE,
  rotate_xlab = FALSE,
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  hline_vars = character(0),
  hline_vars_colors = "green",
  hline_vars_labels = hline_vars,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  font_size = c(12, 8, 20),
  dot_size = c(2, 1, 12),
  alpha = c(0.8, 0, 1),
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
  E.g. `ADaM` structured laboratory data frame `ALB`.

- param_var:

  name of variable containing biomarker codes e.g. `PARAMCD`.

- param:

  list of biomarkers of interest.

- yaxis_var:

  name of variable containing biomarker results displayed on y-axis e.g.
  `AVAL`. When not provided, it defaults to
  `choices_selected(c("AVAL", "CHG"), "AVAL")`.

- xaxis_var:

  variable to categorize the x-axis. When not provided, it defaults to
  `choices_selected("AVISITCD", "AVISITCD")`.

- facet_var:

  variable to facet the plots by. When not provided, it defaults to
  `choices_selected(c("ARM", "ACTARM"), "ARM")`.

- trt_group:

  [`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)
  object with available choices and pre-selected option for variable
  names representing treatment group e.g. `ARM`.

- color_manual:

  vector of colors applied to treatment values.

- shape_manual:

  vector of symbols applied to `LOQ` values.

- facet_ncol:

  numeric value indicating number of facets per row.

- loq_legend:

  `loq` legend toggle.

- rotate_xlab:

  45 degree rotation of `x-axis` values.

- hline_arb:

  numeric vector of at most 2 values identifying intercepts for
  arbitrary horizontal lines.

- hline_arb_color:

  a character vector of at most length of `hline_arb`. naming the color
  for the arbitrary horizontal lines.

- hline_arb_label:

  a character vector of at most length of `hline_arb`. naming the label
  for the arbitrary horizontal lines.

- hline_vars:

  a character vector to name the columns that will define additional
  horizontal lines.

- hline_vars_colors:

  a character vector naming the colors for the additional horizontal
  lines.

- hline_vars_labels:

  a character vector naming the labels for the additional horizontal
  lines that will appear in the legend.

- plot_height:

  controls plot height.

- plot_width:

  optional, controls plot width.

- font_size:

  font size control for title, `x-axis` label, `y-axis` label and
  legend.

- dot_size:

  plot dot size.

- alpha:

  numeric vector to define transparency of plotted points.

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

## Value

an
[`module`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_modules.html)
object

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

Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com

Balazs Toth (tothb2) toth.balazs@gene.com

## Examples

``` r
# Example using ADaM structure analysis dataset.
data <- teal_data()
data <- within(data, {
  library(dplyr)
  library(nestcolor)
  library(stringr)

  # use non-exported function from goshawk
  .h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")

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
      ACTARM = factor(ACTARM) %>% reorder(TRTORD),
      ANRLO = 50,
      ANRHI = 75
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
    tm_g_gh_boxplot(
      label = "Box Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
      yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
      xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
      facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
      trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
      loq_legend = TRUE,
      rotate_xlab = FALSE,
      hline_arb = c(60, 55),
      hline_arb_color = c("grey", "red"),
      hline_arb_label = c("default_hori_A", "default_hori_B"),
      hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      hline_vars_colors = c("pink", "brown", "purple", "black"),
    )
  )
)
#> Initializing tm_g_gh_boxplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
