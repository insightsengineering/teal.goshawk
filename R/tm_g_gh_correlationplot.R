#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description Scatter Plot Teal Module For Biomarker Analysis
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#' laboratory data frame \code{ADLB}.
#' @param param_var `r badge("deprecated")` name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param xaxis_param (`picks` or `choices_selected`) biomarker selected for `x-axis`.
#' @param yaxis_param (`picks` or `choices_selected`) biomarker selected for `y-axis`.
#' @param xaxis_var (`variables` or `choices_selected`)
#' name of variable containing biomarker results displayed on x-axis e.g. \code{BASE}.
#' @param yaxis_var (`variables` or `choices_selected`)
#' name of variable containing biomarker results displayed on y-axis e.g. \code{AVAL}.
#' @param trt_group (`variables` or `choices_selected`) object with available choices and pre-selected option
#' for variable names representing treatment group e.g. `ARM`.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param trt_facet facet by treatment group \code{trt_group}.
#' @param visit_facet visit facet toggle.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param loq_legend `loq` legend toggle.
#' @param rotate_xlab 45 degree rotation of `x-axis` values.
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
#' @param font_size font size control for title, `x-axis` label, `y-axis` label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @export
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @inheritSection teal::example_module Reporting
#'
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(stringr)
#'
#'   # use non-exported function from goshawk
#'   .h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")
#'
#'   # original ARM value = dose value
#'   .arm_mapping <- list(
#'     "A: Drug X" = "150mg QD",
#'     "B: Placebo" = "Placebo",
#'     "C: Combination" = "Combination"
#'   )
#'   .color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#'   # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#'   .shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
#'
#'   set.seed(1) # @linksto ADSL ADLB
#'   ADSL <- rADSL
#'   ADLB <- rADLB
#'   .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'   ADLB <- ADLB %>%
#'     mutate(AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~
#'         paste(
#'           "W",
#'           trimws(
#'             substr(
#'               AVISIT,
#'               start = 6,
#'               stop = str_locate(AVISIT, "DAY") - 1
#'             )
#'           )
#'         ),
#'       TRUE ~ NA_character_
#'     )) %>%
#'     mutate(AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ NA_real_
#'     )) %>%
#'     # use ARMCD values to order treatment in visualization legend
#'     mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'       ifelse(grepl("B", ARMCD), 2,
#'         ifelse(grepl("A", ARMCD), 3, NA)
#'       )
#'     )) %>%
#'     mutate(ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))])) %>%
#'     mutate(ARM = factor(ARM) %>%
#'       reorder(TRTORD)) %>%
#'     mutate(
#'       ANRHI = case_when(
#'         PARAMCD == "ALT" ~ 60,
#'         PARAMCD == "CRP" ~ 70,
#'         PARAMCD == "IGA" ~ 80,
#'         TRUE ~ NA_real_
#'       ),
#'       ANRLO = case_when(
#'         PARAMCD == "ALT" ~ 20,
#'         PARAMCD == "CRP" ~ 30,
#'         PARAMCD == "IGA" ~ 40,
#'         TRUE ~ NA_real_
#'       )
#'     ) %>%
#'     rowwise() %>%
#'     group_by(PARAMCD) %>%
#'     mutate(LBSTRESC = ifelse(
#'       USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'     )) %>%
#'     mutate(LBSTRESC = ifelse(
#'       USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'     )) %>%
#'     ungroup()
#'   attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
#'   attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'   attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'
#'   # add LLOQ and ULOQ variables
#'   ADLB_LOQS <- .h_identify_loq_values(ADLB, "LOQFL")
#'   ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_correlationplot(
#'       label = "Correlation Plot",
#'       dataname = "ADLB",
#'       xaxis_param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "ALT", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       yaxis_param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "CRP", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       xaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
#'       yaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = variables(c("ARM", "ACTARM"), "ARM"),
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
#'       hline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
#'       vline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       vline_vars_colors = c("yellow", "orange", "brown", "gold"),
#'       vline_vars_labels = c("ANRHI Label", "ANRLO Label", "ULOQN Label", "LLOQN Label"),
#'       vline_arb = c(50, 70),
#'       vline_arb_label = "arb vert A",
#'       vline_arb_color = c("green", "orange")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_correlationplot <- function(label,
                                    dataname = "ADLB",
                                    param_var = lifecycle::deprecated(),
                                    xaxis_param = teal.picks::picks(
                                      teal.picks::variables("PARAMCD", "PARAMCD"),
                                      teal.picks::values(selected = "ALT", multiple = FALSE),
                                      check_dataset = FALSE
                                    ),
                                    xaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
                                    yaxis_param = teal.picks::picks(
                                      teal.picks::variables("PARAMCD", "PARAMCD"),
                                      teal.picks::values(selected = "CRP", multiple = FALSE),
                                      check_dataset = FALSE
                                    ),
                                    yaxis_var = teal.picks::variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
                                    trt_group = teal.picks::variables(selected = "ARM"),
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
                                    transformators = list()) {
  message("Initializing tm_g_gh_correlationplot")

  checkmate::assert_string(dataname)

  checkmate::assert_multi_class(xaxis_param, c("choices_selected", "picks"))
  checkmate::assert_multi_class(yaxis_param, c("choices_selected", "picks"))
  checkmate::assert_multi_class(xaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(yaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(trt_group, c("choices_selected", "variables", "picks"))

  checkmate::assert_character(color_manual, null.ok = TRUE, names = "unique")
  checkmate::assert_vector(shape_manual, null.ok = TRUE, names = "unique")
  checkmate::assert_integerish(facet_ncol, lower = 1, len = 1)
  checkmate::assert_flag(visit_facet)
  checkmate::assert_flag(trt_facet)
  checkmate::assert_flag(reg_line)
  checkmate::assert_flag(loq_legend)
  checkmate::assert_flag(rotate_xlab)

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

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

  if (lifecycle::is_present(param_var)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "tm_g_gh_correlationplot(param_var)",
      details = "Please use `teal.picks::picks()` to specificy `xaxis_param` and `yaxis_param` instead of `param_var`."
    )
    checkmate::assert_string(param_var)
  } else {
    param_var <- rlang::maybe_missing(param_var, "PARAMCD")
  }
  param_var <- teal.picks::variables(param_var, param_var)

  if (inherits(xaxis_param, "choices_selected")) {
    xaxis_param <- migrate_choices_selected_to_values(xaxis_param)
    xaxis_param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param_var, xaxis_param)
  } else {
    xaxis_param <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_param)
  }

  xaxis_var <- migrate_choices_selected_to_variables(xaxis_var)
  yaxis_var <- migrate_choices_selected_to_variables(yaxis_var)
  trt_group <- migrate_choices_selected_to_variables(trt_group)

  if (inherits(yaxis_param, "choices_selected")) {
    yaxis_param <- migrate_choices_selected_to_values(yaxis_param)
    yaxis_param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param_var, yaxis_param)
  } else {
    yaxis_param <- create_picks_helper(teal.picks::datasets(dataname, dataname), yaxis_param)
  }

  # These 2 assertions should be moved to section above after "choices_selected" migration is removed
  teal.picks::assert_last_level(xaxis_param, "values")
  teal.picks::assert_last_level(yaxis_param, "values")

  xaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_var)
  yaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), yaxis_var)
  trt_group <- create_picks_helper(teal.picks::datasets(dataname, dataname), trt_group)

  # Defined  per module
  xaxis_param <- force_pick_selection(xaxis_param, which = "values")
  yaxis_param <- force_pick_selection(yaxis_param, which = "values")
  trt_group <- force_pick_selection(trt_group, which = "variables")
  xaxis_var <- force_pick_selection(xaxis_var, which = "variables")
  yaxis_var <- force_pick_selection(yaxis_var, which = "variables")

  args <- as.list(environment())

  module(
    label = label,
    datanames = .picks_datanames(xaxis_param, xaxis_var, yaxis_param, yaxis_var, trt_group),
    server = srv_g_correlationplot,
    server_args = args[names(args) %in% names(formals(srv_g_correlationplot))],
    ui = ui_g_correlationplot,
    ui_args = args[names(args) %in% names(formals(ui_g_correlationplot))],
    transformators = transformators
  )
}

ui_g_correlationplot <- function(id,
                                 dataname,
                                 xaxis_param,
                                 xaxis_var,
                                 yaxis_param,
                                 yaxis_var,
                                 trt_group,
                                 facet_ncol,
                                 visit_facet,
                                 trt_facet,
                                 reg_line,
                                 loq_legend,
                                 rotate_xlab,
                                 hline_arb,
                                 hline_arb_color,
                                 hline_arb_label,
                                 hline_vars,
                                 vline_arb,
                                 vline_arb_color,
                                 vline_arb_label,
                                 vline_vars,
                                 font_size,
                                 dot_size,
                                 reg_text_size,
                                 pre_output,
                                 post_output) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding = tags$div(
      templ_ui_dataname(dataname),
      tmpl_axis_selection_ui(
        ns,
        xaxis_param = xaxis_param,
        xaxis_var = xaxis_var,
        yaxis_param = yaxis_param,
        yaxis_var = yaxis_var,
        trt_group = trt_group
      ),
      templ_ui_constraint(ns, "X-Axis Data Constraint"), # required by constr_anl_q
      if (length(hline_vars) > 0) {
        teal.widgets::optionalSelectInput(
          ns("hline_vars"),
          label = "Add Horizontal Range Line(s):",
          choices = hline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(id = ns("hline_arb"), hline_arb, hline_arb_label, hline_arb_color),
      if (length(vline_vars) > 0) {
        teal.widgets::optionalSelectInput(
          ns("vline_vars"),
          label = "Add Vertical Range Line(s):",
          choices = vline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(
        id = ns("vline_arb"),
        vline_arb,
        vline_arb_label,
        vline_arb_color,
        title = "Arbitrary Vertical Lines:"
      ),
      bslib::accordion(
        bslib::accordion_panel(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(ns("xrange_scale"), label = "X-Axis Range Zoom"),
          toggle_slider_ui(ns("yrange_scale"), label = "Y-Axis Range Zoom"),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", facet_ncol, min = 1),
          checkboxInput(ns("trt_facet"), "Treatment Variable Faceting", trt_facet),
          checkboxInput(ns("visit_facet"), "Visit Faceting", visit_facet),
          checkboxInput(ns("reg_line"), "Regression Line", reg_line),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", rotate_xlab)
        ),
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", dot_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            ns("reg_text_size"),
            "Regression Annotations Size",
            reg_text_size,
            ticks = FALSE
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_g_correlationplot <- function(id,
                                  data,
                                  dataname,
                                  param_var,
                                  xaxis_param,
                                  xaxis_var,
                                  yaxis_param,
                                  yaxis_var,
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
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        xaxis_param = xaxis_param,
        xaxis_var = xaxis_var,
        yaxis_param = yaxis_param,
        yaxis_var = yaxis_var,
        trt_group = trt_group
      ),
      data = data
    )

    xaxis_param_sel <- reactive(selectors$xaxis_param()$values$selected)
    yaxis_param_sel <- reactive(selectors$yaxis_param()$values$selected)
    xaxis_var_sel <- reactive(selectors$xaxis_var()$variables$selected)
    yaxis_var_sel <- reactive(selectors$yaxis_var()$variables$selected)
    trt_group_sel <- reactive(selectors$trt_group()$variables$selected)
    param_var_sel <- reactive(selectors$xaxis_param()$variables$selected) # Should be the same as yaxis_param

    data_with_card <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      teal.code::eval_code(obj, "library(dplyr)")
    })

    validated_q <- reactive({
      validate(
        teal::need_input(
          inputId = c("xaxis_param-variables-selected", "yaxis_param-variables-selected"),
          condition = identical(selectors$xaxis_param()$variables$selected, selectors$yaxis_param()$variables$selected),
          message = "X-Axis and Y-Axis biomarkers must be from the same biomarker variable"
        ),
        teal::need_input(
          inputId = "xaxis_param-values-selected",
          condition = length(xaxis_param_sel()) != 0,
          message = "Please select an X-Axis biomarker"
        ),
        teal::need_input(
          inputId = "yaxis_param-values-selected",
          condition = length(yaxis_param_sel()) != 0,
          message = "Please select a Y-Axis biomarker"
        ),
        teal::need_input(
          inputId = "trt_group-variables-selected",
          condition = length(trt_group_sel()) != 0,
          message = "Please select a treatment variable"
        ),
        teal::need_input(
          inputId = "xaxis_var-variables-selected",
          condition = length(xaxis_var_sel()) != 0,
          message = "Please select an X-Axis variable"
        ),
        teal::need_input(
          inputId = "yaxis_var-variables-selected",
          condition = length(yaxis_var_sel()) != 0,
          message = "Please select a Y-Axis variable"
        ),
        teal::need_input(
          inputId = "facet_ncol",
          condition = length(input$facet_ncol) != 0 && input$facet_ncol > 0 && as.numeric(input$facet_ncol) %% 1 == 0,
          message = "Please select a facet column integer that is greater than 0"
        )
      )
      validate(
        teal::need_input(
          inputId = c("xaxis_param-variables-selected", "yaxis_param-variables-selected"),
          condition = identical(selectors$xaxis_param()$variables$selected, selectors$yaxis_param()$variables$selected),
          message = "X-Axis and Y-Axis biomarkers must be from the same biomarker variable in dataset"
        )
      )
      data_with_card()
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(anl_constraint_output()$iv_r())
      iv$add_validator(horizontal_line()$iv_r())
      iv$add_validator(vertical_line()$iv_r())
      iv$enable()
      iv
    })

    # filter selected biomarkers
    anl_param <- reactive({
      dataset_var <- dataname
      ANL <- validated_q()[[dataname]]
      validate_has_data(ANL, 1)

      if (length(input$hline_vars) > 0) {
        validate(
          teal::need_input(
            inputId = "hline_vars",
            condition = all(input$hline_vars %in% names(ANL)),
            message = "One or more selected horizontal line variable(s) is/are not names to any column in the data"
          ),
          teal::need_input(
            inputId = "vline_vars",
            condition = all(input$vline_vars %in% names(ANL)),
            message = "One or more selected vertical line variable(s) is/are not names to any column in the data"
          )
        )
      }

      validate_has_variable(ANL, param_var_sel())
      validate_has_variable(ANL, "AVISITCD")
      validate_has_variable(ANL, "BASE")
      validate_has_variable(ANL, "BASE2")

      teal::validate_input(
        "xaxis_param",
        length(xaxis_param_sel()) > 0 &&
          length(unique(ANL[[param_var_sel()]])) > 0 &&
          all(xaxis_param_sel() %in% unique(ANL[[param_var_sel()]])),
        sprintf("X-Axis Biomarker %s is not available in data %s", xaxis_param_sel(), dataname)
      )

      teal::validate_input(
        "yaxis_param",
        length(yaxis_param_sel()) > 0 &&
          length(unique(ANL[[param_var_sel()]])) > 0 &&
          all(yaxis_param_sel() %in% unique(ANL[[param_var_sel()]])),
        sprintf("Y-Axis Biomarker %s is not available in data %s", yaxis_param_sel(), dataname)
      )

      lapply(
        list(
          c(
            "AVISITCD", "BASE", "BASE2", "LOQFL", "PARAM", "LBSTRESC",
            trt_group_sel(), "USUBJID", xaxis_var_sel(), yaxis_var_sel(),
            param_var_sel()
          )
        ), function(var) {
          validate_has_variable(ANL, var, sprintf("Variable %s is not available in data %s", var, dataname))
        }
      )

      # analysis
      private_qenv <- within(
        validated_q(),
        {
          ANL <- env_dataset_var_name %>%
            dplyr::filter(.data[[env_param_sel]] %in% union(env_xaxis_param_sel, env_yaxis_param_sel)) %>%
            dplyr::select(dplyr::all_of(env_selected))
        },
        env_dataset_var_name = as.name(dataset_var),
        env_param_sel = param_var_sel(),
        env_xaxis_param_sel = xaxis_param_sel(),
        env_yaxis_param_sel = yaxis_param_sel(),
        env_selected = c(
          "USUBJID", trt_group_sel(), "AVISITCD", param_var_sel(), "PARAM",
          xaxis_var_sel(), yaxis_var_sel(), "AVALU", "LOQFL", "LBSTRESC",
          unique(c(input$hline_vars, input$vline_vars))
        )
      )
      validate_has_data(private_qenv[["ANL"]], 1)
      return(list(ANL = ANL, qenv = private_qenv))
    })

    # constraints
    observe({
      req(xaxis_param_sel())

      constraint_var <- input$constraint_var
      req(constraint_var)

      # note that filtered is false thus we cannot use anl_param()$ANL
      ANL <- validated_q()[[dataname]]
      validate_has_data(ANL, 1)

      ANL <- dplyr::filter(ANL, .data[[param_var_sel()]] == xaxis_param_sel())

      visit_freq <- unique(ANL$AVISITCD)

      # get min max values
      if (
        (constraint_var == "BASE2" && any(grepl("SCR", visit_freq))) ||
          (constraint_var == "BASE" && any(grepl("BL", visit_freq)))
      ) {
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

    anl_constraint_output <- create_anl_constraint_reactive(anl_param, input, param_id = xaxis_param_sel, min_rows = 1)
    anl_constraint <- anl_constraint_output()$value

    # update sliders for axes taking constraints into account
    data_state_x <- reactive({
      get_data_range_states(
        varname = xaxis_var_sel(),
        paramname = xaxis_param_sel(),
        ANL = anl_constraint()$ANL
      )
    })
    xrange_slider <- toggle_slider_server("xrange_scale", data_state_x)
    data_state_y <- reactive({
      get_data_range_states(
        varname = yaxis_var_sel(),
        paramname = yaxis_param_sel(),
        ANL = anl_constraint()$ANL
      )
    })
    yrange_slider <- toggle_slider_server("yrange_scale", data_state_y)

    keep_data_const_opts_updated(session, input, anl_constraint, xaxis_param_sel)

    # selector names after transposition
    xvar <- reactive(paste0(xaxis_var_sel(), "_", xaxis_param_sel()))
    yvar <- reactive(paste0(yaxis_var_sel(), "_", yaxis_param_sel()))
    xloqfl <- reactive(paste0("LOQFL_", xaxis_param_sel()))
    yloqfl <- reactive(paste0("LOQFL_", yaxis_param_sel()))

    # transpose data to plot
    data_transpose_q <- reactive({
      teal::validate_inputs(iv_r())

      req(anl_constraint())
      ANL <- anl_constraint()$ANL

      qenv <- within(
        anl_constraint()$qenv,
        ANL_x <- ANL %>%
          dplyr::filter(.data[[env_param_var_sel]] == env_axis_param_sel & !is.na(.data[[env_xaxis_var_sel]])),
        env_param_var_sel = param_var_sel(),
        env_axis_param_sel = xaxis_param_sel(),
        env_xaxis_var_sel = xaxis_var_sel()
      )

      if (xaxis_var_sel() == "BASE") {
        qenv <- within(qenv, {
          ANL_x <- ANL_x %>%
            dplyr::group_by(.data[["USUBJID"]]) %>%
            dplyr::mutate(LOQFL = .data[["LOQFL"]][.data[["AVISITCD"]] == "BL"]) %>%
            dplyr::ungroup()
        })
      } else if (xaxis_var_sel() != "AVAL") {
        qenv <- within(qenv, {
          ANL_x <- ANL_x %>%
            dplyr::mutate(LOQFL = "N")
        })
      }

      qenv <- within(
        qenv,
        ANL_y <- ANL %>%
          dplyr::filter(.data[[env_param_var_sel]] == env_axis_param_sel & !is.na(.data[[env_yaxis_var_sel]])),
        env_param_var_sel = param_var_sel(),
        env_axis_param_sel = yaxis_param_sel(),
        env_yaxis_var_sel = yaxis_var_sel()
      )

      if (yaxis_var_sel() == "BASE") {
        qenv <- within(qenv, {
          ANL_y <- ANL_y %>%
            dplyr::group_by(.data[["USUBJID"]]) %>%
            dplyr::mutate(LOQFL = .data[["LOQFL"]][.data[["AVISITCD"]] == "BL"]) %>%
            dplyr::ungroup()
        })
      } else if (yaxis_var_sel() != "AVAL") {
        qenv <- within(qenv, {
          ANL_y <- ANL_y %>%
            dplyr::mutate(LOQFL = "N")
        })
      }

      qenv <- within(
        qenv,
        {
          ANL_TRANSPOSED <- dplyr::inner_join(
            ANL_x, ANL_y,
            by = c("USUBJID", "AVISITCD", env_trt_group_sel),
            suffix = env_suffix
          )
          ANL_TRANSPOSED <- ANL_TRANSPOSED %>%
            dplyr::mutate(
              LOQFL_COMB = case_when(
                .data[[env_xloqfl]] == "Y" | .data[[env_yloqfl]] == "Y" ~ "Y",
                .data[[env_xloqfl]] == "N" | .data[[env_yloqfl]] == "N" ~ "N",
                TRUE ~ "NA"
              )
            )
        },
        env_trt_group_sel = trt_group_sel(),
        env_suffix = sprintf("_%s", c(xaxis_param_sel(), yaxis_param_sel())),
        env_xloqfl = xloqfl(),
        env_yloqfl = yloqfl()
      )

      validate(need(nrow(qenv[["ANL_TRANSPOSED"]]) > 0, "Plot Data No Observations Left"))
      validate_has_variable(data = qenv[["ANL_TRANSPOSED"]], varname = c(xvar(), yvar(), xloqfl(), yloqfl()))

      within(qenv,
        {
          attr(ANL_TRANSPOSED[[trt_group_val]], "label") <- attr(ANL[[trt_group_val]], "label")
        },
        trt_group_val = trt_group_sel()
      )
    })

    plot_labels <- reactive({
      req(anl_constraint())
      ANL <- anl_constraint()$qenv[["ANL"]]

      xparam <- ANL$PARAM[ANL[[param_var_sel()]] == xaxis_param_sel()][1]
      yparam <- ANL$PARAM[ANL[[param_var_sel()]] == yaxis_param_sel()][1]

      # setup the x-axis label.  Combine the biomarker and the units (if available)
      if (is.null(ANL$AVALU) || all(ANL[["AVALU"]] == "")) {
        title_text <- paste(xparam, "and", yparam, "@ Visits")
        xaxis_lab <- paste(xparam, xaxis_var_sel(), "Values")
        yaxis_lab <- paste(yparam, yaxis_var_sel(), "Values")
      } else {
        xunit <- ANL$AVALU[ANL[[param_var_sel()]] == xaxis_param_sel()][1]
        yunit <- ANL$AVALU[ANL[[param_var_sel()]] == yaxis_param_sel()][1]

        title_text <- paste0(xparam, " (", xunit, ") and ", yparam, " (", yunit, ") @ Visits")
        xaxis_lab <- paste0(xparam, " (", xunit, ") ", xaxis_var_sel(), " Values")
        yaxis_lab <- paste0(yparam, " (", yunit, ") ", yaxis_var_sel(), " Values")
      }

      list(title_text = title_text, xaxis_lab = xaxis_lab, yaxis_lab = yaxis_lab)
    })

    horizontal_line <- srv_arbitrary_lines("hline_arb")
    vertical_line <- srv_arbitrary_lines("vline_arb")

    # plot
    plot_q <- debounce(reactive({
      req(data_transpose_q())

      validate( # Validation must occur after anl_constraint() has valid data
        teal::need_input(
          inputId = "xrange_scale",
          condition = checkmate::test_numeric(xrange_slider$value, len = 2) &&
            xrange_slider$value[1] < xrange_slider$value[2],
          message = "X-Axis Range Zoom: Invalid range"
        ),
        teal::need_input(
          inputId = "yrange_scale",
          condition = checkmate::test_numeric(yrange_slider$value, len = 2) &&
            yrange_slider$value[1] < yrange_slider$value[2],
          message = "Y-Axis Range Zoom: Invalid range"
        )
      )

      xlim <- xrange_slider$value
      ylim <- yrange_slider$value
      font_size <- input$font_size
      dot_size <- input$dot_size
      reg_text_size <- input$reg_text_size
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      hline_vars <- if (length(input$hline_vars) == 0) {
        NULL
      } else {
        paste0(input$hline_vars, "_", yaxis_param_sel())
      }
      vline_arb <- vertical_line()$line_arb
      vline_arb_label <- vertical_line()$line_arb_label
      vline_arb_color <- vertical_line()$line_arb_color
      vline_vars <- if (length(input$vline_vars) == 0) {
        NULL
      } else {
        paste0(input$vline_vars, "_", xaxis_param_sel())
      }
      facet_ncol <- input$facet_ncol
      visit_facet <- input$visit_facet
      facet <- input$trt_facet
      reg_line <- input$reg_line
      loq_legend <- input$loq_legend
      rotate_xlab <- input$rotate_xlab
      title_text <- plot_labels()$title_text
      xaxis_lab <- plot_labels()$xaxis_lab
      yaxis_lab <- plot_labels()$yaxis_lab
      obj <- data_transpose_q()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("### Plot")
        )

      teal.code::eval_code(
        object = obj,
        code = bquote({
          # re-establish treatment variable label
          p <- goshawk::g_correlationplot(
            data = ANL_TRANSPOSED,
            param_var = .(param_var_sel()),
            xaxis_param = .(xaxis_param_sel()),
            xaxis_var = .(xaxis_var_sel()),
            xvar = .(xvar()),
            yaxis_param = .(yaxis_param_sel()),
            yaxis_var = .(yaxis_var_sel()),
            yvar = .(yvar()),
            trt_group = .(trt_group_sel()),
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
            facet_var = .(trt_group_sel()),
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
            hline_vars_labels = .(paste(hline_vars_labels[seq_along(hline_vars)], "-", yaxis_param_sel())),
            vline_arb = .(vline_arb),
            vline_arb_label = .(vline_arb_label),
            vline_arb_color = .(vline_arb_color),
            vline_vars = .(vline_vars),
            vline_vars_colors = .(vline_vars_colors[seq_along(vline_vars)]),
            vline_vars_labels = .(paste(vline_vars_labels[seq_along(vline_vars)], "-", xaxis_param_sel()))
          )
          p
        })
      )
    }), 800)

    plot_r <- reactive(plot_q()[["p"]])

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    reactive_df <- debounce(reactive({
      req(iv_r()$is_valid())
      plot_brush <- plot_data$brush()

      ANL_TRANSPOSED <- isolate(data_transpose_q()$ANL_TRANSPOSED)

      df <- teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL_TRANSPOSED, "USUBJID", dplyr::all_of(trt_group_sel()), "AVISITCD",
          dplyr::all_of(c(xvar(), yvar())), "LOQFL_COMB"
        ),
        plot_brush
      )
    }), 800)

    # highlight plot area
    output$brush_data <- DT::renderDataTable({
      numeric_cols <- names(dplyr::select_if(reactive_df(), is.numeric))

      DT::datatable(reactive_df(),
        rownames = FALSE, options = list(scrollX = TRUE)
      ) %>%
        DT::formatRound(numeric_cols, 4)
    })

    set_chunk_dims(plot_data, plot_q)
  })
}
