#' Box Plot
#'
#' This teal module renders the UI and calls the functions that create a box plot and accompanying
#' summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#'  laboratory data frame `ADLB`.
#' @param param_var `r badge("deprecated")` name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param (`picks` or `choices_selected`) biomarker selected.
#' @param yaxis_var (`variables` or `choices_selected`) name of variable containing biomarker results displayed on
#'  y-axis e.g. `AVAL`.
#' @param xaxis_var (`variables` or `choices_selected`) variable to categorize the x-axis.
#' @param facet_var (`variables` or `choices_selected`) variable to facet the plots by.
#' @param trt_group (`variables` or `choices_selected`) object with available choices and pre-selected option
#'  for variable names representing treatment group e.g. `ARM`.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values.
#' @param facet_ncol numeric value indicating number of facets per row.
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
#'  in the legend.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, `x-axis` label, `y-axis` label and legend.
#' @param dot_size plot dot size.
#' @param alpha numeric vector to define transparency of plotted points.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#'
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_gh_boxplot(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied only to `plot` output
#'    )
#' )
#' ```
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examplesIf require("nestcolor")
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(nestcolor)
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
#'   ADSL <- teal.data::rADSL
#'   ADLB <- teal.data::rADLB
#'   .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'   ADLB <- ADLB %>%
#'     mutate(
#'       AVISITCD = case_when(
#'         AVISIT == "SCREENING" ~ "SCR",
#'         AVISIT == "BASELINE" ~ "BL",
#'         grepl("WEEK", AVISIT) ~ paste("W", str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'         TRUE ~ as.character(NA)
#'       ),
#'       AVISITCDN = case_when(
#'         AVISITCD == "SCR" ~ -2,
#'         AVISITCD == "BL" ~ 0,
#'         grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'         TRUE ~ as.numeric(NA)
#'       ),
#'       AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'       TRTORD = case_when(
#'         ARMCD == "ARM C" ~ 1,
#'         ARMCD == "ARM B" ~ 2,
#'         ARMCD == "ARM A" ~ 3
#'       ),
#'       ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))]),
#'       ARM = factor(ARM) %>% reorder(TRTORD),
#'       ACTARM = as.character(.arm_mapping[match(ACTARM, names(.arm_mapping))]),
#'       ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'       ANRLO = 50,
#'       ANRHI = 75
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
#'
#'   attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
#'   attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'   attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#'   # add LLOQ and ULOQ variables
#'   ALB_LOQS <- .h_identify_loq_values(ADLB, "LOQFL")
#'   ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_boxplot(
#'       label = "Box Plot",
#'       dataname = "ADLB",
#'       param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "ALT", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       yaxis_var = variables(c("AVAL", "BASE", "CHG"), "AVAL"),
#'       xaxis_var = variables(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
#'       facet_var = variables(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
#'       trt_group = variables(c("ARM", "ACTARM"), "ARM"),
#'       loq_legend = TRUE,
#'       rotate_xlab = FALSE,
#'       hline_arb = c(60, 55),
#'       hline_arb_color = c("grey", "red"),
#'       hline_arb_label = c("default_hori_A", "default_hori_B"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("pink", "brown", "purple", "black"),
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_boxplot <- function(label,
                            dataname = "ADLB",
                            param_var = lifecycle::deprecated(),
                            param = teal.picks::picks(
                              teal.picks::variables("PARAMCD", "PARAMCD"),
                              teal.picks::values(selected = "ALT", multiple = FALSE),
                              check_dataset = FALSE
                            ),
                            yaxis_var = teal.picks::variables(c("AVAL", "CHG"), "AVAL"),
                            xaxis_var = teal.picks::variables("AVISITCD", "AVISITCD"),
                            facet_var = teal.picks::variables(dplyr::starts_with("ARM"), selected = "ARM"),
                            trt_group = teal.picks::variables(selected = "ARM"),
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
                            alpha = c(0.8, 0.0, 1.0),
                            pre_output = NULL,
                            post_output = NULL,
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_g_gh_boxplot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)

  checkmate::assert_multi_class(param, c("choices_selected", "picks"))
  checkmate::assert_multi_class(yaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(xaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(facet_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(trt_group, c("choices_selected", "variables", "picks"))

  checkmate::assert_integerish(facet_ncol, null.ok = TRUE, lower = 1, len = 1)
  checkmate::assert_flag(loq_legend)
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(dot_size, len = 3)
  checkmate::assert_numeric(alpha, len = 3)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_list(transformators)
  checkmate::assert_character(color_manual, null.ok = TRUE)
  checkmate::assert_integerish(shape_manual, null.ok = TRUE)

  teal::assert_decorators(decorators, names = "plot")

  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  if (lifecycle::is_present(param_var)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "tm_g_gh_boxplot(param_var)",
      details = "Please use `teal.picks::picks()` to specify `param` instead of `param_var`."
    )
    checkmate::assert_string(param_var)
  } else {
    param_var <- rlang::maybe_missing(param_var, "PARAMCD")
  }
  param_var <- teal.picks::variables(param_var, param_var)

  if (inherits(param, "choices_selected")) {
    param <- migrate_choices_selected_to_values(param)
    param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param_var, param)
  } else {
    param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param)
  }

  yaxis_var <- migrate_choices_selected_to_variables(yaxis_var)
  xaxis_var <- migrate_choices_selected_to_variables(xaxis_var)
  facet_var <- migrate_choices_selected_to_variables(facet_var)
  trt_group <- migrate_choices_selected_to_variables(trt_group)

  teal.picks::assert_last_level(param, "values")

  yaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), yaxis_var)
  xaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_var)
  facet_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), facet_var)
  trt_group <- create_picks_helper(teal.picks::datasets(dataname, dataname), trt_group)

  param <- force_pick_selection(param, which = "values")
  yaxis_var <- force_pick_selection(yaxis_var, which = "variables")
  xaxis_var <- force_pick_selection(xaxis_var, which = "variables")
  facet_var <- force_pick_selection(facet_var, which = "variables")
  trt_group <- force_pick_selection(trt_group, which = "variables")

  args <- as.list(environment())

  module(
    label = label,
    datanames = .picks_datanames(param, yaxis_var, xaxis_var, facet_var, trt_group),
    server = srv_g_boxplot,
    server_args = args[names(args) %in% names(formals(srv_g_boxplot))],
    ui = ui_g_boxplot,
    ui_args = args[names(args) %in% names(formals(ui_g_boxplot))],
    transformators = transformators
  )
}

ui_g_boxplot <- function(id,
                         dataname,
                         param,
                         yaxis_var,
                         xaxis_var,
                         facet_var,
                         trt_group,
                         facet_ncol,
                         loq_legend,
                         rotate_xlab,
                         hline_arb,
                         hline_arb_color,
                         hline_arb_label,
                         hline_vars,
                         font_size,
                         dot_size,
                         alpha,
                         pre_output,
                         post_output,
                         decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = bslib::page_fluid(
      tags$div(
        teal.widgets::plot_with_settings_ui(id = ns("boxplot"))
      ),
      tags$div(
        tags$br(), tags$hr(),
        tags$h4("Selected Data Points"),
        DT::dataTableOutput(ns("brush_data"))
      ),
      tags$div(
        tags$br(), tags$hr(),
        tags$h4("Descriptive Statistics"),
        DT::dataTableOutput(ns("table_ui"))
      )
    ),
    encoding = tags$div(
      templ_ui_dataname(dataname),
      tmpl_axis_selection_ui(
        ns,
        xaxis_param = param,
        xaxis_var = xaxis_var,
        yaxis_var = yaxis_var,
        facet_var = facet_var,
        trt_group = trt_group,
        xparam_label = "Select a Biomarker"
      ),
      templ_ui_constraint(ns, label = "Data Constraint"),
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
      teal::ui_transform_teal_data("decorator", select_decorators(decorators, "plot")),
      bslib::accordion(
        bslib::accordion_panel(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(ns("yrange_scale"), label = "Y-Axis Range Zoom"),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", facet_ncol, min = 1),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", rotate_xlab)
        ),
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", dot_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Dot Alpha", alpha, ticks = FALSE)
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}


srv_g_boxplot <- function(id,
                          data,
                          dataname,
                          param_var,
                          param,
                          yaxis_var,
                          xaxis_var,
                          facet_var,
                          trt_group,
                          color_manual,
                          shape_manual,
                          plot_height,
                          plot_width,
                          hline_vars_colors,
                          hline_vars_labels,
                          decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        xaxis_param = param,
        yaxis_var = yaxis_var,
        xaxis_var = xaxis_var,
        facet_var = facet_var,
        trt_group = trt_group
      ),
      data = data
    )

    param_sel <- reactive(selectors$xaxis_param()$values$selected)
    yaxis_var_sel <- reactive(selectors$yaxis_var()$variables$selected)
    xaxis_var_sel <- reactive(selectors$xaxis_var()$variables$selected)
    facet_var_sel <- reactive(selectors$facet_var()$variables$selected)
    trt_group_sel <- reactive(selectors$trt_group()$variables$selected)
    param_var_sel <- reactive(selectors$xaxis_param()$variables$selected)

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
          inputId = "xaxis_param-values-selected",
          condition = length(param_sel()) != 0,
          message = "Please select a biomarker"
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
          condition = checkmate::test_integerish(input$facet_ncol, lower = 1, null.ok = TRUE),
          message = "Please select a facet column integer that is greater than 0"
        )
      )
      validate(
        teal::need_input(
          inputId = c("xaxis_var-variables-selected", "trt_group-variables-selected"),
          condition = isFALSE(xaxis_var_sel() %in% c("ACTARM", "ARM")) || isTRUE(xaxis_var_sel() == trt_group_sel()),
          message = sprintf(
            "You can not choose %s as x-axis variable for treatment variable %s.", xaxis_var_sel(), trt_group_sel()
          )
        ),
        teal::need_input(
          inputId = c("facet_var-variables-selected", "trt_group-variables-selected"),
          condition = isFALSE(facet_var_sel() %in% c("ACTARM", "ARM")) || isTRUE(facet_var_sel() == trt_group_sel()),
          message = sprintf(
            "You can not choose %s as faceting variable for treatment variable %s.", facet_var_sel(), trt_group_sel()
          )
        )
      )
      data_with_card()
    })

    anl_q_output <- constr_anl_q(
      session, input, validated_q, dataname,
      param_r = param_sel, param_var_r = param_var_sel, trt_group_r = trt_group_sel, min_rows = 2
    )

    anl_q <- anl_q_output()$value

    # update sliders for axes taking constraints into account
    data_state <- reactive({
      get_data_range_states(
        varname = yaxis_var_sel(),
        paramname = param_sel(),
        ANL = anl_q()$ANL
      )
    })
    yrange_slider <- toggle_slider_server("yrange_scale", data_state)
    keep_data_const_opts_updated(session, input, anl_q, param_sel)

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(horizontal_line()$iv_r())
      iv$add_validator(anl_q_output()$iv_r())
      iv$enable()
      iv
    })

    create_plot <- debounce(reactive({
      teal::validate_inputs(iv_r())
      req(anl_q())

      facet_var_val <- if (is.null(facet_var_sel()) || length(facet_var_sel()) == 0) {
        "None"
      } else {
        facet_var_sel()
      }

      validate( # Validation must occur after anl_constraint() has valid data
        teal::need_input(
          inputId = "yrange_scale",
          condition = checkmate::test_numeric(yrange_slider$value, len = 2) &&
            yrange_slider$value[1] < yrange_slider$value[2],
          message = "Y-Axis Range Zoom: Invalid range"
        )
      )
      ylim <- yrange_slider$value
      facet_ncol <- input$facet_ncol

      alpha <- input$alpha
      font_size <- input$font_size
      dot_size <- input$dot_size
      loq_legend <- input$loq_legend
      rotate_xlab <- input$rotate_xlab

      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color

      hline_vars <- input$hline_vars

      validate_has_variable(
        anl_q()$ANL,
        yaxis_var_sel(),
        sprintf("Variable %s is not available in data %s", yaxis_var_sel(), dataname)
      )
      validate_has_variable(
        anl_q()$ANL,
        xaxis_var_sel(),
        sprintf("Variable %s is not available in data %s", xaxis_var_sel(), dataname)
      )

      if (!facet_var_val == "None") {
        validate_has_variable(
          anl_q()$ANL,
          facet_var_val,
          sprintf("Variable %s is not available in data %s", facet_var_val, dataname)
        )
      }

      obj <- anl_q()$qenv

      constraint_description <- c(
        paste("\nFacet By:", facet_var_val),
        paste("\nSelect an X-axis Variable:", xaxis_var_sel())
      )

      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)"),
          teal.reporter::teal_card(
            "### Selected Options",
            formatted_data_constraint(
              constraint_var = param_sel(),
              constraint_range_min = input$constraint_range_min,
              constraint_range_max = input$constraint_range_max
            ),
            constraint_description
          ),
          teal.reporter::teal_card("### Plot")
        )

      obj %>% teal.code::eval_code(
        code = bquote({
          plot <- goshawk::g_boxplot(
            data = ANL,
            biomarker = .(param_sel()),
            xaxis_var = .(xaxis_var_sel()),
            yaxis_var = .(yaxis_var_sel()),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars)]),
            facet_ncol = .(facet_ncol),
            loq_legend = .(loq_legend),
            rotate_xlab = .(rotate_xlab),
            trt_group = .(trt_group_sel()),
            ylim = .(ylim),
            color_manual = .(color_manual),
            shape_manual = .(shape_manual),
            facet_var = .(facet_var_val),
            alpha = .(alpha),
            dot_size = .(dot_size),
            font_size = .(font_size),
            unit = .("AVALU")
          )
        })
      )
    }), 800)

    decorated_plot_q <- teal::srv_transform_teal_data(
      "decorators",
      create_plot,
      select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(decorated_plot_q()[["plot"]])

    create_table <- debounce(reactive({
      req(iv_r()$is_valid())
      req(anl_q())
      font_size <- input$font_size
      facet_var_val <- `if`(is.null(facet_var_sel()) || length(facet_var_sel()) == 0, "None", facet_var_sel())

      anl_q()$qenv %>% teal.code::eval_code(
        code = bquote({
          tbl <- goshawk::t_summarytable(
            data = ANL,
            trt_group = .(trt_group_sel()),
            param_var = .(param_var_sel()),
            param = .(param_sel()),
            xaxis_var = .(yaxis_var_sel()),
            facet_var = .(facet_var_val)
          )
          tbl
        })
      )
    }), 800)

    boxplot_data <- teal.widgets::plot_with_settings_srv(
      id = "boxplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    output$table_ui <- DT::renderDataTable({
      req(create_table())
      tbl <- create_table()[["tbl"]]

      numeric_cols <- setdiff(names(dplyr::select_if(tbl, is.numeric)), "n")

      DT::datatable(tbl,
        rownames = FALSE, options = list(scrollX = TRUE)
      ) %>%
        DT::formatRound(numeric_cols, 4)
    })

    joined_qenvs <- reactive({
      req(decorated_plot_q(), create_table())
      c(decorated_plot_q(), create_table())
    })

    # highlight plot area
    reactive_df <- debounce(reactive({
      boxplot_brush <- boxplot_data$brush()

      ANL <- isolate(anl_q()$ANL) %>% droplevels()
      validate_has_data(ANL, 2)

      xvar <- isolate(xaxis_var_sel())
      yvar <- isolate(yaxis_var_sel())
      facetv <- isolate(facet_var_sel())
      trt_group_val <- isolate(trt_group_sel())

      req(all(c(xvar, yvar, trt_group_val) %in% names(ANL)))

      if (!is.null(facetv) && length(facetv) > 0) {
        req(facetv %in% names(ANL))
      }

      teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL, "USUBJID", dplyr::all_of(c(trt_group_val, facetv)),
          "AVISITCD", "PARAMCD", dplyr::all_of(c(xvar, yvar)), "LOQFL"
        ),
        boxplot_brush
      )
    }), 800)

    output$brush_data <- DT::renderDataTable({
      numeric_cols <- names(dplyr::select_if(reactive_df(), is.numeric))

      DT::datatable(reactive_df(),
        rownames = FALSE, options = list(scrollX = TRUE)
      ) %>%
        DT::formatRound(numeric_cols, 4)
    })

    set_chunk_dims(boxplot_data, joined_qenvs)
  })
}
