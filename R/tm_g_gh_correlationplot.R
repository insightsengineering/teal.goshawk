#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description Scatter Plot Teal Module For Biomarker Analysis
#'
#' @inheritParams teal.widgets::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured laboratory data frame
#'   \code{ADLB}.
#' @param param_var name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param xaxis_param biomarker selected for x-axis.
#' @param yaxis_param biomarker selected for y-axis.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. \code{BASE}.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. \code{AVAL}.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param trt_facet facet by treatment group \code{trt_group}.
#' @param visit_facet visit facet toggle.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param loq_legend loq legend toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param hline_vars a character vector to name the columns that will define additional horizontal lines.
#' @param hline_vars_colors a character vector naming the colors for the additional horizontal lines.
#' @param hline_vars_labels a character vector naming the labels for the additional horizontal lines that will appear
#' @param vline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param vline_arb_color a character vector of at most length of \code{vline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param vline_arb_label a character vector of at most length of \code{vline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param vline_vars a character vector to name the columns that will define additional vertical lines.
#' @param vline_vars_colors a character vector naming the colors for the additional vertical lines.
#' @param vline_vars_labels a character vector naming the labels for the additional vertical lines that will appear
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @export
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#' library(scda)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#' color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
#'
#' cached_data <- synthetic_cdisc_data("latest")
#' ADSL <- cached_data$adsl
#'
#' set.seed(1)
#' ADLB <- cached_data$adlb
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   dplyr::mutate(AVISITCD = dplyr::case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~
#'       paste(
#'         "W",
#'         trimws(
#'           substr(
#'             AVISIT,
#'             start = 6,
#'             stop = stringr::str_locate(AVISIT, "DAY") - 1
#'           )
#'         )
#'       ),
#'     TRUE ~ NA_character_
#'   )) %>%
#'   dplyr::mutate(AVISITCDN = dplyr::case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0,
#'     grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'     TRUE ~ NA_real_
#'   )) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   dplyr::mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)
#'     )
#'   )) %>%
#'   dplyr::mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   dplyr::mutate(ARM = factor(ARM) %>%
#'     reorder(TRTORD)) %>%
#'   dplyr::mutate(
#'     ANRHI = dplyr::case_when(
#'       PARAMCD == "ALT" ~ 60,
#'       PARAMCD == "CRP" ~ 70,
#'       PARAMCD == "IGA" ~ 80,
#'       TRUE ~ NA_real_
#'     ),
#'     ANRLO = dplyr::case_when(
#'       PARAMCD == "ALT" ~ 20,
#'       PARAMCD == "CRP" ~ 30,
#'       PARAMCD == "IGA" ~ 40,
#'       TRUE ~ NA_real_
#'     )
#'   ) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::group_by(PARAMCD) %>%
#'   dplyr::mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'   )) %>%
#'   dplyr::mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'   )) %>%
#'   ungroup()
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#' attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'
#' # add LLOQ and ULOQ variables
#' ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#' ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "set.seed(1)
#'               ADLB <- synthetic_cdisc_data('latest')$adlb
#'               var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'               ADLB <- ADLB %>%
#'                 dplyr::mutate(AVISITCD = dplyr::case_when(
#'                   AVISIT == 'SCREENING' ~ 'SCR',
#'                   AVISIT == 'BASELINE' ~ 'BL',
#'                   grepl('WEEK', AVISIT) ~
#'                     paste(
#'                       'W',
#'                       trimws(
#'                         substr(
#'                           AVISIT,
#'                           start = 6,
#'                           stop = stringr::str_locate(AVISIT, 'DAY') - 1
#'                         )
#'                       )
#'                     ),
#'                   TRUE ~ NA_character_)) %>%
#'                 dplyr::mutate(AVISITCDN = dplyr::case_when(
#'                   AVISITCD == 'SCR' ~ -2,
#'                   AVISITCD == 'BL' ~ 0,
#'                   grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'                   TRUE ~ NA_real_)) %>%
#'                 # use ARMCD values to order treatment in visualization legend
#'                 dplyr::mutate(TRTORD = ifelse(grepl('C', ARMCD), 1,
#'                                        ifelse(grepl('B', ARMCD), 2,
#'                                               ifelse(grepl('A', ARMCD), 3, NA)))) %>%
#'                 dplyr::mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'                 dplyr::mutate(ARM = factor(ARM) %>%
#'                          reorder(TRTORD)) %>%
#'                 dplyr::mutate(
#'                   ANRHI = dplyr::case_when(
#'                     PARAMCD == 'ALT' ~ 60,
#'                     PARAMCD == 'CRP' ~ 70,
#'                     PARAMCD == 'IGA' ~ 80,
#'                     TRUE ~ NA_real_
#'                   ),
#'                   ANRLO = dplyr::case_when(
#'                     PARAMCD == 'ALT' ~ 20,
#'                     PARAMCD == 'CRP' ~ 30,
#'                     PARAMCD == 'IGA' ~ 40,
#'                     TRUE ~ NA_real_
#'                   )) %>%
#'                 dplyr::rowwise() %>%
#'                 dplyr::group_by(PARAMCD) %>%
#'                 dplyr::mutate(LBSTRESC = ifelse(
#'                   USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'                   paste('<', round(runif(1, min = 25, max = 30))), LBSTRESC)) %>%
#'                 dplyr::mutate(LBSTRESC = ifelse(
#'                   USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'                   paste( '>', round(runif(1, min = 70, max = 75))), LBSTRESC)) %>%
#'                 ungroup()
#'               attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'               attr(ADLB[['ANRHI']], 'label') <- 'Analysis Normal Range Upper Limit'
#'               attr(ADLB[['ANRLO']], 'label') <- 'Analysis Normal Range Lower Limit'
#'
#'               # add LLOQ and ULOQ variables
#'               ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#'               ADLB <- left_join(ADLB, ADLB_LOQS, by = 'PARAM')",
#'       vars = list(arm_mapping = arm_mapping)
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_gh_correlationplot(
#'       label = "Correlation Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       xaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       yaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "CRP"),
#'       xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       color_manual = c(
#'         "Drug X 100mg" = "#000000",
#'         "Placebo" = "#3498DB",
#'         "Combination 100mg" = "#E74C3C"
#'       ),
#'       shape_manual = c("N" = 1, "Y" = 2, "NA" = 0),
#'       plot_height = c(500, 200, 2000),
#'       facet_ncol = 2,
#'       visit_facet = TRUE,
#'       reg_line = FALSE,
#'       loq_legend = TRUE,
#'       font_size = c(12, 8, 20),
#'       dot_size = c(1, 1, 12),
#'       reg_text_size = c(3, 3, 10),
#'       hline_arb = c(40, 50),
#'       hline_arb_label = "arb hori label",
#'       hline_arb_color = c("red", "blue"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("green", "blue", "purple", "cyan"),
#'       hline_vars_label = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
#'       vline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       vline_vars_colors = c("yellow", "orange", "brown", "gold"),
#'       vline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
#'       vline_arb = c(50, 70),
#'       vline_arb_label = "arb vert A",
#'       vline_arb_color = c("green", "orange")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_gh_correlationplot <- function(label,
                                    dataname,
                                    param_var = "PARAMCD",
                                    xaxis_param = "ALT",
                                    xaxis_var = "BASE",
                                    yaxis_param = "CRP",
                                    yaxis_var = "AVAL",
                                    trt_group,
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
                                    post_output = NULL) {
  logger::log_info("Initializing tm_g_gh_correlationplot")
  checkmate::assert_class(xaxis_param, "choices_selected")
  checkmate::assert_class(yaxis_param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_flag(trt_facet)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_arb_arg(vline_arb, vline_arb_color, vline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
  validate_line_vars_arg(vline_vars, vline_vars_colors, vline_vars_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(dot_size, len = 3)
  checkmate::assert_numeric(reg_text_size, len = 3)

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_correlationplot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      trt_group = trt_group,
      trt_facet = trt_facet,
      color_manual = color_manual,
      shape_manual = shape_manual,
      plot_height = plot_height,
      plot_width = plot_width,
      hline_vars_colors = hline_vars_colors,
      hline_vars_labels = hline_vars_labels,
      vline_vars_colors = vline_vars_colors,
      vline_vars_labels = vline_vars_labels
    ),
    ui = ui_g_correlationplot,
    ui_args = args
  )
}

ui_g_correlationplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      templ_ui_dataname(a$dataname),
      teal.widgets::optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = a$trt_group$choices,
        selected = a$trt_group$selected,
        multiple = FALSE
      ),
      templ_ui_params_vars(
        ns,
        xparam_choices = a$xaxis_param$choices, xparam_selected = a$xaxis_param$selected,
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected,
        yparam_choices = a$yaxis_param$choices, yparam_selected = a$yaxis_param$selected,
        ychoices = a$yaxis_var$choices, yselected = a$yaxis_var$selected
      ),
      templ_ui_constraint(ns, "X-Axis Data Constraint"), # required by constr_anl_chunks
      if (length(a$hline_vars) > 0) {
        teal.widgets::optionalSelectInput(
          ns("hline_vars"),
          label = "Add Horizontal Range Line(s):",
          choices = a$hline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(id = ns("hline_arb"), a$hline_arb, a$hline_arb_label, a$hline_arb_color),
      if (length(a$vline_vars) > 0) {
        teal.widgets::optionalSelectInput(
          ns("vline_vars"),
          label = "Add Vertical Range Line(s):",
          choices = a$vline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(
        id = ns("vline_arb"),
        a$vline_arb,
        a$vline_arb_label,
        a$vline_arb_color,
        title = "Arbitrary Vertical Lines:"
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("xrange_scale"),
            label = "X-Axis Range Zoom",
            min = -1000000, max = 1000000, value = c(-1000000, 1000000)
          ),
          toggle_slider_ui(
            ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000, max = 1000000, value = c(-1000000, 1000000)
          ),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("trt_facet"), "Treatment Variable Faceting", a$trt_facet),
          checkboxInput(ns("visit_facet"), "Visit Faceting", a$visit_facet),
          checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", a$loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab)
        ),
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            ns("reg_text_size"),
            "Regression Annotations Size",
            a$reg_text_size,
            ticks = FALSE
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_correlationplot <- function(id,
                                  datasets,
                                  reporter,
                                  dataname,
                                  param_var,
                                  trt_group,
                                  trt_facet,
                                  color_manual,
                                  shape_manual,
                                  plot_height,
                                  plot_width,
                                  hline_vars_colors,
                                  hline_vars_labels,
                                  vline_vars_colors,
                                  vline_vars_labels) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")

  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    # filter selected biomarkers
    anl_param <- reactive({
      validate(need(input$trt_group, "Please select a Treatment Variable"))
      validate(need(input$xaxis_param, "Please select an X-Axis Biomarker"))
      validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
      validate(need(input$yaxis_param, "Please select a Y-Axis Biomarker"))
      validate(need(input$yaxis_var, "Please select a Y-Axis Variable"))

      dataset_var <- dataname
      ANL <- datasets$get_data(dataname, filtered = TRUE) # nolint
      validate_has_data(ANL, 1)

      if (length(input$hline_vars) > 0) {
        validate(
          need(
            all(input$hline_vars %in% names(ANL)),
            "One or more selected horizontal line variable(s) is/are not names to any column in the data"
          ),
          need(
            all(input$vline_vars %in% names(ANL)),
            "One or more selected vertical line variable(s) is/are not names to any column in the data"
          )
        )
      }

      validate_has_variable(ANL, param_var)

      validate_in(
        input$xaxis_param, unique(ANL[[param_var]]),
        sprintf("X-Axis Biomarker %s is not available in data %s", input$xaxis_param, dataname)
      )

      validate_in(
        input$yaxis_param, unique(ANL[[param_var]]),
        sprintf("Y-Axis Biomarker %s is not available in data %s", input$yaxis_param, dataname)
      )

      validate_has_variable(
        ANL,
        "AVISITCD",
        sprintf("Variable AVISITCD is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        "BASE",
        sprintf("Variable BASE is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        "BASE2",
        sprintf("Variable BASE2 is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        "LOQFL",
        sprintf("Variable LOQFL is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        "PARAM",
        sprintf("Variable PARAM is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        "LBSTRESC",
        sprintf("Variable LBSTRESC is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        input$trt_group,
        sprintf("Variable %s is not available in data %s", input$trt_group, dataname)
      )

      validate_has_variable(
        ANL,
        "USUBJID",
        sprintf("Variable USUBJID is not available in data %s", dataname)
      )

      validate_has_variable(
        ANL,
        input$xaxis_var,
        sprintf("Variable %s is not available in data %s", input$xaxis_var, dataname)
      )

      validate_has_variable(
        ANL,
        input$yaxis_var,
        sprintf("Variable %s is not available in data %s", input$yaxis_var, dataname)
      )

      # analysis
      private_chunks <- teal.code::chunks_new()
      teal.code::chunks_reset(as.environment(stats::setNames(list(ANL), dataset_var)), private_chunks)

      # filter biomarker
      teal.code::chunks_push(
        chunks = private_chunks,
        id = "filter_biomarker",
        expression = bquote({
          ANL <- .(as.name(dataset_var)) %>% # nolint
            dplyr::filter(.data[[.(param_var)]] %in% union(.(input$xaxis_param), .(input$yaxis_param)))
        })
      )

      ANL <- teal.code::chunks_safe_eval(private_chunks) # nolint
      validate_has_data(ANL, 1)

      return(list(ANL = ANL, chunks = private_chunks))
    })

    # constraints
    observe({
      validate(need(input$xaxis_param, "Please select an X-Axis Biomarker"))

      constraint_var <- input$constraint_var
      validate(need(constraint_var, "select a constraint variable"))

      # note that filtered is false thus we cannot use anl_param()$ANL
      ANL <- datasets$get_data(dataname, filtered = FALSE) # nolint

      validate_has_variable(ANL, param_var)
      validate_has_variable(ANL, "AVISITCD")
      validate_has_variable(ANL, "BASE")
      validate_has_variable(ANL, "BASE2")

      ANL <- ANL %>% dplyr::filter(.data[[param_var]] == input$xaxis_param) # nolint

      visit_freq <- unique(ANL$AVISITCD)

      # get min max values
      if ((constraint_var == "BASE2" && any(grepl("SCR", visit_freq))) ||
        (constraint_var == "BASE" && any(grepl("BL", visit_freq)))) {
        val <- stats::na.omit(switch(constraint_var,
          "BASE" = ANL$BASE[ANL$AVISITCD == "BL"],
          "BASE2" = ANL$BASE2[ANL$AVISITCD == "SCR"],
          stop(paste(constraint_var, "not allowed"))
        ))

        if (length(val) == 0 || all(is.na(val))) {
          shinyjs::show("all_na")
          shinyjs::hide("constraint_range")
          args <- list(
            min = list(label = "Min", min = 0, max = 0, value = 0),
            max = list(label = "Max", min = 0, max = 0, value = 0)
          )
          update_min_max(session, args)
        } else {
          rng <- range(val, na.rm = TRUE)

          minmax <- c(floor(rng[1] * 1000) / 1000, ceiling(rng[2] * 1000) / 1000)

          label_min <- sprintf("Min (%s)", minmax[1])
          label_max <- sprintf("Max (%s)", minmax[2])

          args <- list(
            min = list(label = label_min, min = minmax[1], max = minmax[2], value = minmax[1]),
            max = list(label = label_max, min = minmax[1], max = minmax[2], value = minmax[2])
          )

          update_min_max(session, args)
          shinyjs::show("constraint_range") # update before show
          shinyjs::hide("all_na")
        }
      } else if (constraint_var == "NONE") {
        shinyjs::hide("constraint_range") # hide before update
        shinyjs::hide("all_na")

        # force update (and thus refresh) on different constraint_var -> pass unique value for each constraint_var name
        args <- list(
          min = list(label = "Min", min = 0, max = 0, value = 0),
          max = list(label = "Max", min = 0, max = 0, value = 0)
        )

        update_min_max(session, args)
      } else {
        stop("invalid contraint_var", constraint_var)
      }
    })

    anl_constraint <- create_anl_constraint_reactive(anl_param, input, param_id = "xaxis_param", min_rows = 1)

    # update sliders for axes taking constraints into account
    xrange_slider <- toggle_slider_server("xrange_scale")
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(session, input, xrange_slider$update_state, "xaxis_var", "xaxis_param", anl_constraint)
    keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "yaxis_param", anl_constraint)
    keep_data_const_opts_updated(session, input, anl_constraint, "xaxis_param")

    # selector names after transposition
    xvar <- reactive(paste0(input$xaxis_var, ".", input$xaxis_param))
    yvar <- reactive(paste0(input$yaxis_var, ".", input$yaxis_param))
    xloqfl <- reactive(paste0("LOQFL_", input$xaxis_param))
    yloqfl <- reactive(paste0("LOQFL_", input$yaxis_param))

    # transpose data to plot
    plot_data_transpose <- reactive({
      private_chunks <- teal.code::chunks_deep_clone(anl_constraint()$chunks)
      ANL <- anl_constraint()$ANL # nolint
      trt_group <- input$trt_group
      line_vars <- unique(c(input$hline_vars, input$vline_vars))

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "plot_data_transpose",
        expression = bquote({
          ANL_TRANSPOSED1 <- ANL %>% # nolint
            dplyr::select(
              .data[["USUBJID"]],
              .data[[.(trt_group)]],
              .data[["AVISITCD"]],
              .data[[.(param_var)]],
              .data[[.(input$xaxis_var)]],
              .data[[.(input$yaxis_var)]],
              .(`if`(length(line_vars) == 0, NULL, line_vars))
            ) %>%
            tidyr::pivot_longer(
              c(
                .data[[.(input$xaxis_var)]],
                .data[[.(input$yaxis_var)]],
                .(`if`(length(line_vars) == 0, NULL, line_vars))
              ),
              names_to = "ANLVARS",
              values_to = "ANLVALS"
            ) %>%
            tidyr::unite(
              "ANL.PARAM",
              "ANLVARS",
              .(param_var),
              sep = ".",
              remove = TRUE
            ) %>%
            tidyr::pivot_wider(names_from = "ANL.PARAM", values_from = "ANLVALS") %>%
            dplyr::filter(!is.na(.data[[.(xvar())]]) & !is.na(.data[[.(yvar())]]))

          ANL_TRANSPOSED2 <- ANL %>% # nolint
            dplyr::select(
              .data[["USUBJID"]],
              .data[[.(trt_group)]],
              .data[["AVISITCD"]],
              .data[[.(param_var)]],
              .data[["LOQFL"]],
              .data[["PARAM"]],
              .data[["LBSTRESC"]]
            ) %>%
            tidyr::pivot_longer(
              c(
                .data[["LOQFL"]],
                .data[["PARAM"]],
                .data[["LBSTRESC"]]
              ),
              names_to = "ANLVARS",
              values_to = "ANLVALS"
            ) %>%
            tidyr::unite(
              "ANL.PARAM",
              "ANLVARS",
              .(param_var),
              sep = "_",
              remove = TRUE
            ) %>%
            tidyr::pivot_wider(names_from = "ANL.PARAM", values_from = "ANLVALS") %>%
            dplyr::mutate(LOQFL_COMB = dplyr::case_when(
              .data[[.(xloqfl())]] == "Y" | .data[[.(yloqfl())]] == "Y" ~ "Y",
              .data[[.(xloqfl())]] == "N" & .data[[.(yloqfl())]] == "N" ~ "N",
              .data[[.(xloqfl())]] == "N" & .data[[.(yloqfl())]] == "NA" ~ "N",
              .data[[.(xloqfl())]] == "NA" & .data[[.(yloqfl())]] == "N" ~ "N",
              .data[[.(xloqfl())]] == "NA" & .data[[.(yloqfl())]] == "NA" ~ "NA",
              TRUE ~ as.character(NA)
            ))

          ANL_TRANSPOSED <- merge(ANL_TRANSPOSED1, ANL_TRANSPOSED2) # nolint
        })
      )

      ANL_TRANSPOSED <- teal.code::chunks_safe_eval(private_chunks) # nolint
      teal.code::chunks_push_new_line(private_chunks)

      validate(need(nrow(ANL_TRANSPOSED) > 0, "Plot Data No Observations Left"))
      validate_has_variable(data = ANL_TRANSPOSED, varname = c(xvar(), yvar(), xloqfl(), yloqfl()))

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "ANL_attributes",
        expression =
          bquote(attr(ANL_TRANSPOSED[[.(trt_group)]], "label") <- attr(ANL[[.(trt_group)]], "label")) # nolint
      )
      teal.code::chunks_push_new_line(private_chunks)

      return(list(ANL_TRANSPOSED = ANL_TRANSPOSED, chunks = private_chunks))
    })

    plot_labels <- reactive({
      ANL <- teal.code::chunks_get_var(var = "ANL", anl_constraint()$chunks) # nolint

      xparam <- ANL$PARAM[ANL[[param_var]] == input$xaxis_param][1]
      yparam <- ANL$PARAM[ANL[[param_var]] == input$yaxis_param][1]

      # setup the x-axis label.  Combine the biomarker and the units (if available)
      if (is.null(ANL$AVALU) || all(ANL[["AVALU"]] == "")) {
        title_text <- paste(xparam, "and", yparam, "@ Visits")
        xaxis_lab <- paste(xparam, input$xaxis_var, "Values")
        yaxis_lab <- paste(yparam, input$yaxis_var, "Values")
      } else {
        xunit <- ANL$AVALU[ANL[[param_var]] == input$xaxis_param][1]
        yunit <- ANL$AVALU[ANL[[param_var]] == input$yaxis_param][1]

        title_text <- paste0(xparam, " (", xunit, ") and ", yparam, " (", yunit, ") @ Visits")
        xaxis_lab <- paste0(xparam, " (", xunit, ") ", input$xaxis_var, " Values")
        yaxis_lab <- paste0(yparam, " (", yunit, ") ", input$yaxis_var, " Values")
      }

      list(title_text = title_text, xaxis_lab = xaxis_lab, yaxis_lab = yaxis_lab)
    })

    horizontal_line <- srv_arbitrary_lines("hline_arb")
    vertical_line <- srv_arbitrary_lines("vline_arb")

    # plot
    plot_r <- reactive({
      private_chunks <- teal.code::chunks_deep_clone(plot_data_transpose()$chunks)
      # nolint start
      xaxis_param <- input$xaxis_param
      xaxis_var <- input$xaxis_var
      yaxis_param <- input$yaxis_param
      yaxis_var <- input$yaxis_var
      xlim <- xrange_slider$state()$value
      ylim <- yrange_slider$state()$value
      font_size <- input$font_size
      dot_size <- input$dot_size
      reg_text_size <- input$reg_text_size
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      hline_vars <- if (length(input$hline_vars) == 0) {
        NULL
      } else {
        paste0(input$hline_vars, ".", yaxis_param)
      }
      vline_arb <- vertical_line()$line_arb
      vline_arb_label <- vertical_line()$line_arb_label
      vline_arb_color <- vertical_line()$line_arb_color
      vline_vars <- if (length(input$vline_vars) == 0) {
        NULL
      } else {
        paste0(input$vline_vars, ".", xaxis_param)
      }
      facet_ncol <- input$facet_ncol
      validate(need(
        is.na(facet_ncol) || (as.numeric(facet_ncol) > 0 && as.numeric(facet_ncol) %% 1 == 0),
        "Number of plots per row must be a positive integer"
      ))
      visit_facet <- input$visit_facet
      facet <- input$trt_facet
      reg_line <- input$reg_line
      loq_legend <- input$loq_legend
      rotate_xlab <- input$rotate_xlab
      # nolint end
      title_text <- plot_labels()$title_text
      xaxis_lab <- plot_labels()$xaxis_lab
      yaxis_lab <- plot_labels()$yaxis_lab
      validate(need(input$trt_group, "Please select a treatment variable"))
      trt_group <- input$trt_group

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "scatterplot",
        expression = bquote({
          # re-establish treatment variable label
          p <- goshawk::g_correlationplot(
            data = ANL_TRANSPOSED,
            param_var = .(param_var),
            xaxis_param = .(xaxis_param),
            xaxis_var = .(xaxis_var),
            xvar = .(xvar()),
            yaxis_param = .(yaxis_param),
            yaxis_var = .(yaxis_var),
            yvar = .(yvar()),
            trt_group = .(trt_group),
            xlim = .(xlim),
            ylim = .(ylim),
            title_text = .(title_text),
            xaxis_lab = .(xaxis_lab),
            yaxis_lab = .(yaxis_lab),
            color_manual = .(color_manual),
            shape_manual = .(shape_manual),
            facet_ncol = .(facet_ncol),
            visit_facet = .(visit_facet),
            facet = .(facet),
            facet_var = .(trt_group),
            reg_line = .(reg_line),
            font_size = .(font_size),
            dot_size = .(dot_size),
            reg_text_size = .(reg_text_size),
            loq_legend = .(loq_legend),
            rotate_xlab = .(rotate_xlab),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(paste(hline_vars_labels[seq_along(hline_vars)], "-", yaxis_param)),
            vline_arb = .(vline_arb),
            vline_arb_label = .(vline_arb_label),
            vline_arb_color = .(vline_arb_color),
            vline_vars = .(vline_vars),
            vline_vars_colors = .(vline_vars_colors[seq_along(vline_vars)]),
            vline_vars_labels = .(paste(vline_vars_labels[seq_along(vline_vars)], "-", xaxis_param))
          )
          print(p)
        })
      )

      teal.code::chunks_safe_eval(private_chunks)

      # promote chunks to be visible in the sessionData by other modules
      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(private_chunks)
      teal.code::chunks_get_var("p")
    })


    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )


    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Correlation Plot")
        card$append_text("Correlation Plot", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Selected Options", "header3")
        card$append_text(
          paste(
            formatted_data_constraint(input$constraint_var, input$constraint_range_min, input$constraint_range_max),
            "\nTreatment Variable Faceting:",
            input$trt_facet,
            "\nRegression Line:",
            input$reg_line
          ),
          style = "verbatim"
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 2L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###

    # highlight plot area
    output$brush_data <- DT::renderDataTable({
      plot_brush <- plot_data$brush()

      ANL_TRANSPOSED <- isolate(plot_data_transpose()$ANL_TRANSPOSED) # nolint

      df <- teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL_TRANSPOSED, "USUBJID", dplyr::all_of(input$trt_group), "AVISITCD",
          dplyr::all_of(c(xvar(), yvar())), "LOQFL_COMB"
        ),
        plot_brush
      )

      numeric_cols <- names(dplyr::select_if(df, is.numeric))

      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = "Correlation Plot"
    )
  })
}
