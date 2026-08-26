#' Line plot
#'
#' This teal module renders the UI and calls the function that creates a line plot.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#' laboratory data frame `ADLB`.
#' @param param_var `r badge("deprecated")` name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param (`picks` or `choices_selected`) biomarker selected.
#' @param param_var_label single name of variable in analysis data that includes parameter labels.
#' @param xaxis_var (`variables` or `choices_selected`)
#' single name of variable in analysis data that is used as x-axis in the plot.
#' @param xvar_level vector that can be used to define the factor level of `xvar`. Only use it when
#' `xaxis_var` is of type character or factor.
#' @param filter_var `r badge("deprecated")` data constraint variable.
#' @param filter_var_choices `r badge("deprecated")` data constraint variable choices.
#' @param yaxis_var (`variables` or `choices_selected`)
#' single name of variable in analysis data that is used as summary variable in the
#' respective `goshawk` function.
#' @param trt_group (`variables` or `choices_selected`) object with available choices and pre-selected option
#' for variable names representing treatment group e.g. `ARM`.
#' @param trt_group_level vector that can be used to define factor level of `trt_group`.
#' @param shape_choices Vector or \code{choices_selected} object with names of `ADSL` variables which
#' can be used to change shape
#' @param color_manual string vector representing customized colors
#' @param stat string of statistics
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric.
#' Default value is waive().
#' @param xlabel vector with same length of `xtick` to define the label of x-axis tick values.
#' Default value is waive().
#' @param rotate_xlab `logical(1)` value indicating whether to rotate `x-axis` labels.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param plot_font_size control font size for title, `x-axis`, `y-axis` and legend font.
#' @param dodge controls the position dodge of error bar
#' @param count_threshold minimum count of observations (as listed in the output table) to plot
#' nodes on the graph
#' @param table_font_size controls the font size of values in the table
#' @param dot_size plot dot size.
#' @param plot_relative_height_value numeric value between 500 and 5000 for controlling the starting value
#' of the relative plot height slider
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
#' tm_g_gh_lineplot(
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
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return \code{shiny} object
#'
#' @export
#'
#' @examplesIf require("nestcolor")
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(stringr)
#'   library(nestcolor)
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
#'       ACTARM = factor(ACTARM) %>% reorder(TRTORD)
#'     )
#'   attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'       param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "ALT", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       shape_choices = c("SEX", "RACE"),
#'       xaxis_var = variables("AVISITCD", "AVISITCD"),
#'       yaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = variables(c("ARM", "ACTARM"), "ARM"),
#'       hline_arb = c(20.5, 19.5),
#'       hline_arb_color = c("red", "green"),
#'       hline_arb_label = c("A", "B")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_lineplot <- function(label,
                             dataname = "ADLB",
                             param_var = lifecycle::deprecated(),
                             param = teal.picks::picks(
                               teal.picks::variables("PARAMCD", "PARAMCD"),
                               teal.picks::values(selected = "ALT", multiple = FALSE),
                               check_dataset = FALSE
                             ),
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
                             color_manual = c(
                               getOption("ggplot2.discrete.colour"),
                               c("#ff0000", "#008000", "#4ca3dd", "#8a2be2")
                             )[1:4],
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
                             decorators = list()) {
  message("Initializing tm_g_gh_lineplot")

  if (lifecycle::is_present(filter_var)) {
    lifecycle::deprecate_stop("0.6.0", "tm_g_gh_lineplot(filter_var)", details = "Variable has been removed.")
  }

  if (lifecycle::is_present(filter_var_choices)) {
    lifecycle::deprecate_stop("0.6.0", "tm_g_gh_lineplot(filter_var_choices)", details = "Variable has been removed.")
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_multi_class(param, c("choices_selected", "picks"))
  checkmate::assert_string(param_var_label)

  checkmate::assert_multi_class(xaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(yaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_character(xvar_level, null.ok = TRUE, names = "named")

  checkmate::assert_multi_class(trt_group, c("choices_selected", "variables", "picks"))
  checkmate::assert_character(trt_group_level, null.ok = TRUE, names = "named")

  checkmate::assert_multi_class(shape_choices, c("choices_selected", "variables", "character"), null.ok = TRUE)
  checkmate::assert_string(stat)

  checkmate::assert_character(hline_arb_color)
  checkmate::assert_character(hline_arb_label)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)

  checkmate::assert_character(color_manual, null.ok = TRUE)
  checkmate::assert(.var.name = "xtick", checkmate::check_class(xtick, "waiver"), checkmate::check_numeric(xtick))
  checkmate::assert(.var.name = "xlabel", checkmate::check_class(xlabel, "waiver"), checkmate::check_character(xlabel))
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_numeric(plot_font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(dodge, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_number(count_threshold)
  checkmate::assert_numeric(table_font_size, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(dot_size, len = 3)
  checkmate::assert_numeric(
    table_font_size[1],
    lower = table_font_size[2], upper = table_font_size[3],
    null.ok = TRUE, .var.name = "table_font_size"
  )
  checkmate::assert_number(plot_relative_height_value, lower = 500, upper = 5000)
  checkmate::assert_list(transformators, types = "teal_transform_module")

  teal::assert_decorators(decorators, names = "plot")

  if (lifecycle::is_present(param_var)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "tm_g_gh_lineplot(param_var)",
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

  xaxis_var <- migrate_choices_selected_to_variables(xaxis_var)
  yaxis_var <- migrate_choices_selected_to_variables(yaxis_var)
  trt_group <- migrate_choices_selected_to_variables(trt_group)

  teal.picks::assert_last_level(param, "values")

  shape_choices <- if (is.character(shape_choices)) {
    teal.picks::variables(shape_choices, NULL, multiple = FALSE)
  } else {
    migrate_choices_selected_to_variables(shape_choices, null.ok = TRUE)
  }

  xaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_var)
  yaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), yaxis_var)
  trt_group <- create_picks_helper(teal.picks::datasets(dataname, dataname), trt_group)

  param <- force_pick_selection(param, which = "values")
  trt_group <- force_pick_selection(trt_group, which = "variables")
  xaxis_var <- force_pick_selection(xaxis_var, which = "variables")
  yaxis_var <- force_pick_selection(yaxis_var, which = "variables")

  if (!is.null(shape_choices)) {
    shape_choices <- create_picks_helper(teal.picks::datasets(dataname, dataname), shape_choices)
    shape_choices <- force_pick_selection(shape_choices, which = "variables")
    attr(shape_choices$variables, "allow-clear") <- TRUE # Enable clearance of selection
  }

  args <- as.list(environment())

  module(
    label = label,
    datanames = .picks_datanames(param, xaxis_var, yaxis_var, trt_group),
    server = srv_lineplot,
    server_args = args[names(args) %in% names(formals(srv_lineplot))],
    ui = ui_lineplot,
    ui_args = args[names(args) %in% names(formals(ui_lineplot))],
    transformators = transformators
  )
}

ui_lineplot <- function(id,
                        dataname,
                        param,
                        xaxis_var,
                        yaxis_var,
                        trt_group,
                        shape_choices,
                        stat,
                        rotate_xlab,
                        hline_arb,
                        hline_arb_color,
                        hline_arb_label,
                        plot_relative_height_value,
                        count_threshold,
                        dodge,
                        plot_font_size,
                        dot_size,
                        table_font_size,
                        pre_output,
                        post_output,
                        decorators) {
  ns <- NS(id)

  shiny::tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::plot_with_settings_ui(id = ns("plot")),
      encoding = tags$div(
        templ_ui_dataname(dataname),
        tmpl_axis_selection_ui(
          ns,
          xaxis_param = param,
          xaxis_var = xaxis_var,
          yaxis_var = yaxis_var,
          trt_group = trt_group,
          xparam_label = "Select a Biomarker"
        ),
        if (!is.null(shape_choices)) {
          tagList(
            tags$strong("Select Line Splitting Variable"),
            teal.picks::picks_ui(ns("shape_choices"), shape_choices)
          )
        },
        radioButtons(ns("stat"), "Select a Statistic:", c("mean", "median"), stat),
        checkboxInput(ns("include_stat"), "Include Statistic Table", value = TRUE),
        tags$div(
          sliderInput(
            ns("relative_height"),
            tags$div(
              "Relative height of plot to table(s)",
              bslib::tooltip(
                trigger = icon("circle-info"),
                tags$span(
                  paste(
                    "The larger the value selected the greater the size of the plot relative\nto",
                    "the size of the tables. Note the units of this slider are arbitrary.\nTo",
                    "change the total size of the plot and table(s)\nuse",
                    "the plot resizing controls available at the top right of the plot."
                  )
                )
              )
            ),
            min = 500,
            max = 5000,
            step = 50,
            value = plot_relative_height_value,
            ticks = FALSE
          ),
        ),
        templ_ui_constraint(ns),
        ui_arbitrary_lines(id = ns("hline_arb"), hline_arb, hline_arb_label, hline_arb_color),
        teal::ui_transform_teal_data(ns("decorator"), select_decorators(decorators, "plot")),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Plot Aesthetic Settings",
            toggle_slider_ui(
              ns("yrange_scale"),
              label = "Y-Axis Range Zoom"
            ),
            checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", rotate_xlab),
            numericInput(ns("count_threshold"), "Contributing Observations Threshold:", count_threshold)
          ),
          bslib::accordion_panel(
            title = "Plot settings",
            teal.widgets::optionalSliderInputValMinMax(ns("dodge"), "Error Bar Position Dodge", dodge, ticks = FALSE),
            bslib::accordion(
              bslib::accordion_panel(
                title = "Line Settings",
                uiOutput(ns("lines"))
              ),
              bslib::accordion_panel(
                title = "Symbol settings",
                uiOutput(ns("symbols"))
              )
            ),
            teal.widgets::optionalSliderInputValMinMax(
              ns("plot_font_size"),
              "Font Size",
              plot_font_size,
              ticks = FALSE
            ),
            teal.widgets::optionalSliderInputValMinMax(
              ns("dot_size"),
              "Dot Size",
              dot_size,
              ticks = FALSE
            )
          ),
          bslib::accordion_panel(
            title = "Table settings",
            teal.widgets::optionalSliderInputValMinMax(
              ns("table_font_size"),
              "Table Font Size",
              table_font_size,
              ticks = FALSE
            )
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

srv_lineplot <- function(id,
                         data,
                         dataname,
                         param_var,
                         param,
                         xaxis_var,
                         yaxis_var,
                         trt_group,
                         color_manual,
                         xvar_level,
                         trt_group_level,
                         shape_choices,
                         param_var_label,
                         xtick,
                         xlabel,
                         plot_height,
                         plot_width,
                         decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")
    ns <- session$ns

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        xaxis_param = param,
        xaxis_var = xaxis_var,
        yaxis_var = yaxis_var,
        trt_group = trt_group,
        shape_choices = shape_choices
      ),
      data = data
    )

    param_sel <- reactive(selectors$xaxis_param()$values$selected)
    xaxis_var_sel <- reactive(selectors$xaxis_var()$variables$selected)
    yaxis_var_sel <- reactive(selectors$yaxis_var()$variables$selected)
    trt_group_sel <- reactive(selectors$trt_group()$variables$selected)
    param_var_sel <- reactive(selectors$xaxis_param()$variables$selected)
    shape_choices_sel <- if (is.null(shape_choices)) {
      function() NULL
    } else {
      reactive(selectors$shape_choices()$variables$selected)
    }

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
        )
      )
      data_with_card()
    })

    anl_q_output <- constr_anl_q(
      session = session,
      input = input,
      data = validated_q,
      dataname = dataname,
      param_r = param_sel,
      param_var_r = param_var_sel,
      trt_group_r = trt_group_sel,
      min_rows = 2
    )

    anl_q <- anl_q_output()$value

    keep_data_const_opts_updated(session, input, anl_q, param_sel)

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(horizontal_line()$iv_r())
      iv$add_validator(anl_q_output()$iv_r())
      iv$enable()
      iv
    })

    data_state <- reactive({
      varname <- yaxis_var_sel()
      validate(need(varname, "Please select variable"))

      ANL <- anl_q()$ANL
      validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

      shape <- shape_choices_sel()

      sum_data <- ANL %>%
        dplyr::group_by_at(c(xaxis_var_sel(), trt_group_sel(), shape)) %>%
        dplyr::summarise(
          upper = if (input$stat == "mean") {
            mean(!!sym(varname), na.rm = TRUE) +
              1.96 * stats::sd(!!sym(varname), na.rm = TRUE) / sqrt(dplyr::n())
          } else {
            stats::quantile(!!sym(varname), 0.75, na.rm = TRUE)
          },
          lower = if (input$stat == "mean") {
            mean(!!sym(varname), na.rm = TRUE) -
              1.96 * stats::sd(!!sym(varname), na.rm = TRUE) / sqrt(dplyr::n())
          } else {
            stats::quantile(!!sym(varname), 0.25, na.rm = TRUE)
          }
        )

      minmax <- grDevices::extendrange(
        r = c(
          floor(min(sum_data$lower, na.rm = TRUE) * 10) / 10,
          ceiling(max(sum_data$upper, na.rm = TRUE) * 10) / 10
        ),
        f = 0.05
      )

      # we don't use get_data_range_states because this module computes the data ranges
      # not from the constrained ANL, but rather by first grouping and computing confidence
      # intervals
      list(
        range = c(min = minmax[[1]], max = minmax[[2]])
      )
    })
    yrange_slider <- toggle_slider_server("yrange_scale", data_state)

    line_color_defaults <- color_manual
    line_type_defaults <- c(
      "blank",
      "solid",
      "dashed",
      "dotted",
      "dotdash",
      "longdash",
      "twodash",
      "1F",
      "F1",
      "4C88C488",
      "12345678"
    )

    line_color_selected <- reactive({
      req(trt_group_sel())
      anl_arm <- as.factor(isolate(anl_q())$ANL[[trt_group_sel()]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)

      stats::setNames(
        vapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- input[[paste0("line_color_", idx)]]
            anl_arm_level <- anl_arm_levels[[idx]]
            if (length(x)) {
              x
            } else if (anl_arm_level %in% names(line_color_defaults)) {
              line_color_defaults[[anl_arm_level]]
            } else if (idx <= length(line_color_defaults)) {
              line_color_defaults[[idx]]
            } else {
              "#000000"
            }
          },
          character(1)
        ),
        anl_arm_levels
      )
    })
    line_type_selected <- reactive({
      req(trt_group_sel())
      anl_arm <- as.factor(isolate(anl_q())$ANL[[trt_group_sel()]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)

      stats::setNames(
        vapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- input[[paste0("line_type_", idx)]]
            if (is.null(x)) "solid" else x
          },
          character(1)
        ),
        anl_arm_levels
      )
    })

    output$lines <- renderUI({
      req(trt_group_sel())
      anl_arm <- as.factor(anl_q()$ANL[[trt_group_sel()]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)

      tagList(
        lapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- anl_arm_levels[[idx]]
            color_input <- colourpicker::colourInput(
              ns(paste0("line_color_", idx)),
              "Color:",
              isolate(line_color_selected()[[idx]])
            )
            type_input <- selectInput(
              ns(paste0("line_type_", idx)),
              "Type:",
              choices = line_type_defaults,
              selected = isolate(line_type_selected()[[idx]])
            )
            tags$div(
              tags$label("Line configuration for:", tags$code(x)),
              tags$div(
                color_input,
                type_input
              )
            )
          }
        )
      )
    })

    symbol_type_start <- c(
      "circle",
      "square",
      "diamond",
      "triangle",
      "circle open",
      "square open",
      "diamond open",
      "triangle open",
      "triangle down open",
      "circle cross",
      "square cross",
      "circle plus",
      "square plus",
      "diamond plus",
      "square triangle",
      "plus",
      "cross",
      "asterisk"
    )
    symbol_type_defaults <- reactiveVal(symbol_type_start)

    # reset shapes when different splitting variable is selected
    observeEvent(
      eventExpr = shape_choices_sel(),
      handlerExpr = symbol_type_defaults(symbol_type_start),
      ignoreNULL = TRUE
    )

    observe({
      req(shape_choices_sel())
      req(anl_q())
      anl_shape <- anl_q()$ANL[[shape_choices_sel()]]
      anl_shape_nlevels <- nlevels(anl_shape)
      symbol_type_to_set <- symbol_type_defaults()[pmin(length(symbol_type_defaults()), seq_len(anl_shape_nlevels))]
      symbol_type_defaults(symbol_type_to_set)
    })

    symbol_type_selected <- reactive({
      req(anl_q())
      if (is.null(shape_choices_sel())) {
        return(NULL)
      }
      anl_shape <- isolate(anl_q()$ANL[[shape_choices_sel()]])
      anl_shape_nlevels <- nlevels(anl_shape)
      anl_shape_levels <- levels(anl_shape)

      stats::setNames(
        vapply(
          seq_len(anl_shape_nlevels),
          function(idx) {
            x <- input[[paste0("symbol_type_", idx)]]
            if (is.null(x)) isolate(symbol_type_defaults())[[idx]] else x
          },
          character(1)
        ),
        anl_shape_levels
      )
    })

    output$symbols <- renderUI({
      req(symbol_type_defaults())
      validate(need(shape_choices_sel(), "Please select line splitting variable first."))

      anl_shape <- isolate(anl_q()$ANL[[shape_choices_sel()]])
      validate(need(is.factor(anl_shape), "Line splitting variable must be a factor."))

      anl_shape_nlevels <- nlevels(anl_shape)
      anl_shape_levels <- levels(anl_shape)
      symbol_def <- symbol_type_defaults()

      tagList(
        lapply(
          seq_len(anl_shape_nlevels),
          function(idx) {
            x <- anl_shape_levels[[idx]]
            x_color <- symbol_def[[idx]]
            selectInput(
              ns(paste0("symbol_type_", idx)),
              HTML(paste0("Symbol for: ", tags$code(x))),
              choices = symbol_type_start,
              selected = x_color
            )
          }
        )
      )
    })

    plot_q <- debounce(reactive({
      teal::validate_inputs(iv_r())
      req(anl_q(), line_color_selected(), line_type_selected())
      validate( # Validation must occur after anl_constraint() has valid data
        teal::need_input(
          inputId = "yrange_scale",
          condition = checkmate::test_numeric(yrange_slider$value, len = 2) &&
            yrange_slider$value[1] < yrange_slider$value[2],
          message = "Y-Axis Range Zoom: Invalid range"
        )
      )
      ylim <- yrange_slider$value
      plot_font_size <- input$plot_font_size
      dot_size <- input$dot_size
      dodge <- input$dodge
      rotate_xlab <- input$rotate_xlab
      count_threshold <- `if`(is.na(as.numeric(input$count_threshold)), 0, as.numeric(input$count_threshold))
      table_font_size <- input$table_font_size

      median <- ifelse(input$stat == "median", TRUE, FALSE)
      relative_height <- input$relative_height
      trt_group_val <- trt_group_sel()
      color_selected <- line_color_selected()
      type_selected <- line_type_selected()
      symbol_selected <- symbol_type_selected()
      include_stat <- input$include_stat

      shape <- shape_choices_sel()

      validate(
        need(
          nrow(anl_q()$ANL[stats::complete.cases(anl_q()$ANL[, c(yaxis_var_sel(), xaxis_var_sel())]), ]) >= 2,
          "Number of complete rows on x and y axis variables is less than 2"
        )
      )

      private_qenv <- anl_q()$qenv

      if (!methods::is(xtick, "waiver") && !is.null(xtick)) {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = bquote({
            keep_index <- which(.(xtick) %in% ANL[[.(xaxis_var_sel())]])
            xtick <- (.(xtick))[keep_index] # extra parentheses needed for edge case, e.g. 1:5[keep_index]
            xlabel <- (.(xlabel))[keep_index]
          })
        )
      } else if (methods::is(xtick, "waiver")) {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = "xtick <- ggplot2::waiver();xlabel <- ggplot2::waiver()"
        )
      }

      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color

      obj <- private_qenv
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("### Plot")
        )

      teal.code::eval_code(
        object = obj,
        code = bquote({
          plot <- goshawk::g_lineplot(
            data = ANL[stats::complete.cases(ANL[, c(.(yaxis_var_sel()), .(xaxis_var_sel()))]), ],
            biomarker_var = .(param_var_sel()),
            biomarker_var_label = .(param_var_label),
            biomarker = .(param_sel()),
            value_var = .(yaxis_var_sel()),
            ylim = .(ylim),
            trt_group = .(trt_group_val),
            trt_group_level = .(trt_group_level),
            shape = .(shape),
            shape_type = .(symbol_selected),
            time = .(xaxis_var_sel()),
            time_level = .(xvar_level),
            color_manual = .(color_selected),
            line_type = .(type_selected),
            median = .(median),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            xtick = .(if (!is.null(xtick)) quote(xtick) else xtick),
            xlabel = .(if (!is.null(xtick)) quote(xlabel) else xlabel),
            rotate_xlab = .(input$rotate_xlab),
            plot_height = .(relative_height),
            plot_font_size = .(input$plot_font_size),
            dot_size = .(input$dot_size),
            dodge = .(input$dodge),
            count_threshold = .(input$count_threshold),
            table_font_size = .(input$table_font_size),
            display_center_tbl = .(include_stat)
          )
        })
      )
    }), 800)

    decorated_plot_q <- teal::srv_transform_teal_data(
      "decorator",
      plot_q,
      select_decorators(decorators, "plot"),
      expr = quote(plot)
    )
    plot_r <- reactive(decorated_plot_q()[["plot"]])

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
    )

    set_chunk_dims(plot_data, decorated_plot_q)
  })
}
