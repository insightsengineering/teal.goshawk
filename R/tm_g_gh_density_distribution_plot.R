#' Density Distribution Plot
#'
#' This teal module renders the UI and calls the functions that create a density distribution plot
#' and an accompanying summary table.
#'
#' @inheritParams tm_g_gh_correlationplot
#' @inheritParams tm_g_gh_lineplot
#' @param color_comb name or hex value for combined treatment color.
#' @param line_size plot line thickness.
#' @param comb_line display combined treatment line toggle.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams teal::module
#'
#' @return A [teal::module()] object that can be used in a [teal::init()] call.
#'
#' @author Nick Paszty
#' @author Balazs Toth
#'
#' @inheritSection teal::example_module Reporting
#'
#' @export
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(stringr)
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
#'
#'   attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
#' })
#'
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_density_distribution_plot(
#'       label = "Density Distribution Plot",
#'       dataname = "ADLB",
#'       param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "ALT", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       xaxis_var = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = variables(c("ARM", "ACTARM"), "ARM"),
#'       color_manual = c(
#'         "150mg QD" = "#000000",
#'         "Placebo" = "#3498DB",
#'         "Combination" = "#E74C3C"
#'       ),
#'       color_comb = "#39ff14",
#'       comb_line = TRUE,
#'       plot_height = c(500, 200, 2000),
#'       font_size = c(12, 8, 20),
#'       line_size = c(1, .25, 3),
#'       hline_arb = c(.02, .05),
#'       hline_arb_color = c("red", "black"),
#'       hline_arb_label = c("Horizontal Line A", "Horizontal Line B")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_density_distribution_plot <- function(label, # nolint: object_length_linter.
                                              dataname = "ADLB",
                                              param_var = lifecycle::deprecated(),
                                              param = teal.picks::picks(
                                                teal.picks::variables("PARAMCD", "PARAMCD"),
                                                teal.picks::values(selected = "ALT", multiple = FALSE),
                                                check_dataset = FALSE
                                              ),
                                              xaxis_var = teal.picks::variables(
                                                c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"
                                              ),
                                              trt_group = teal.picks::variables(
                                                dplyr::starts_with("ARM"),
                                                selected = "ARM"
                                              ),
                                              color_manual = NULL,
                                              color_comb = NULL,
                                              plot_height = c(500, 200, 2000),
                                              plot_width = NULL,
                                              font_size = c(12, 8, 20),
                                              line_size = c(1, .25, 3),
                                              hline_arb = numeric(0),
                                              hline_arb_color = "red",
                                              hline_arb_label = "Horizontal line",
                                              facet_ncol = 2L,
                                              comb_line = TRUE,
                                              rotate_xlab = FALSE,
                                              pre_output = NULL,
                                              post_output = NULL,
                                              transformators = list()) {
  message("Initializing tm_g_gh_density_distribution_plot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)

  checkmate::assert_multi_class(param, c("choices_selected", "picks"))
  checkmate::assert_multi_class(xaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(trt_group, c("choices_selected", "variables", "picks"))

  checkmate::assert_character(color_manual, null.ok = TRUE, any.missing = FALSE, unique = TRUE, names = "named")
  checkmate::assert_string(color_comb, null.ok = TRUE)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(line_size, len = 3)

  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)

  checkmate::assert_integerish(facet_ncol, lower = 1, len = 1)
  checkmate::assert_flag(comb_line)
  checkmate::assert_flag(rotate_xlab)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_list(transformators, types = "teal_transform_module")

  if (lifecycle::is_present(param_var)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "tm_g_gh_density_distribution_plot(param_var)",
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
  trt_group <- migrate_choices_selected_to_variables(trt_group)

  teal.picks::assert_last_level(param, "values")

  xaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_var)
  trt_group <- create_picks_helper(teal.picks::datasets(dataname, dataname), trt_group)

  param <- force_pick_selection(param, which = "values")
  trt_group <- force_pick_selection(trt_group, which = "variables")
  xaxis_var <- force_pick_selection(xaxis_var, which = "variables")

  args <- as.list(environment())

  module(
    label = label,
    datanames = .picks_datanames(param, xaxis_var, trt_group),
    server = srv_g_density_distribution_plot,
    server_args = args[names(args) %in% names(formals(srv_g_density_distribution_plot))],
    ui = ui_g_density_distribution_plot,
    ui_args = args[names(args) %in% names(formals(ui_g_density_distribution_plot))],
    transformators = transformators
  )
}

ui_g_density_distribution_plot <- function(id,
                                           dataname,
                                           param,
                                           xaxis_var,
                                           trt_group,
                                           facet_ncol,
                                           comb_line,
                                           rotate_xlab,
                                           hline_arb,
                                           hline_arb_color,
                                           hline_arb_label,
                                           font_size,
                                           line_size,
                                           pre_output,
                                           post_output) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = bslib::page_fluid(
      tags$div(
        teal.widgets::plot_with_settings_ui(id = ns("plot"))
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
        trt_group = trt_group,
        xparam_label = "Select a Biomarker"
      ),
      templ_ui_constraint(ns, label = "Data Constraint"),
      ui_arbitrary_lines(id = ns("hline_arb"), hline_arb, hline_arb_label, hline_arb_color),
      bslib::accordion(
        bslib::accordion_panel(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(ns("xrange_scale"), label = "X-Axis Range Zoom"),
          toggle_slider_ui(ns("yrange_scale"), label = "Y-Axis Range Zoom"),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", facet_ncol, min = 1),
          checkboxInput(ns("comb_line"), "Display Combined line", comb_line),
          checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", rotate_xlab)
        ),
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            ns("line_size"),
            "Line Size",
            value_min_max = line_size,
            step = .25,
            ticks = FALSE
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_g_density_distribution_plot <- function(id, # nolint: object_length_linter.
                                            data,
                                            dataname,
                                            param_var,
                                            param,
                                            xaxis_var,
                                            trt_group,
                                            color_manual,
                                            color_comb,
                                            plot_height,
                                            plot_width) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        xaxis_param = param,
        xaxis_var = xaxis_var,
        trt_group = trt_group
      ),
      data = data
    )

    param_sel <- reactive(selectors$xaxis_param()$values$selected)
    xaxis_var_sel <- reactive(selectors$xaxis_var()$variables$selected)
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
    data_state_x <- reactive({
      get_data_range_states(
        varname = xaxis_var_sel(),
        paramname = param_sel(),
        ANL = anl_q()$ANL
      )
    })
    xrange_slider <- toggle_slider_server("xrange_scale", data_state_x)
    data_state_y <- reactive({
      get_data_range_states(
        varname = xaxis_var_sel(),
        paramname = param_sel(),
        ANL = anl_q()$ANL,
        trt_group = trt_group_sel()
      )
    })
    yrange_slider <- toggle_slider_server("yrange_scale", data_state_y)

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
      line_size <- input$line_size
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      facet_ncol <- input$facet_ncol

      comb_line <- input$comb_line
      rug_plot <- input$rug_plot
      rotate_xlab <- input$rotate_xlab

      obj <- anl_q()$qenv
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("### Plot")
        )

      teal.code::eval_code(
        object = obj,
        code = bquote({
          p <- goshawk::g_density_distribution_plot(
            data = ANL,
            param_var = .(param_var_sel()),
            param = .(param_sel()),
            xaxis_var = .(xaxis_var_sel()),
            trt_group = .(trt_group_sel()),
            xlim = .(xlim),
            ylim = .(ylim),
            color_manual = .(color_manual),
            color_comb = .(color_comb),
            font_size = .(font_size),
            line_size = .(line_size),
            facet_ncol = .(facet_ncol),
            comb_line = .(comb_line),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            rug_plot = .(rug_plot)
          )
          p
        })
      )
    }), 800)

    plot_r <- reactive({
      create_plot()[["p"]]
    })

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
    )

    create_plot_dims <- set_chunk_dims(plot_data, create_plot)

    create_table <- debounce(reactive({
      req(iv_r()$is_valid())
      req(create_plot_dims())
      font_size <- input$font_size

      obj <- create_plot_dims()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "### Descriptive Statistics")
      teal.code::eval_code(
        object = obj,
        code = bquote({
          tbl <- goshawk::t_summarytable(
            data = ANL,
            trt_group = .(trt_group_sel()),
            param_var = .(param_var_sel()),
            param = .(param_sel()),
            xaxis_var = .(xaxis_var_sel()),
            font_size = .(font_size)
          )
          tbl
        })
      )
    }), 800)

    output$table_ui <- DT::renderDataTable({
      req(create_table())
      tbl <- create_table()[["tbl"]]
      numeric_cols <- names(dplyr::select_if(tbl, is.numeric))

      DT::datatable(tbl,
        rownames = FALSE, options = list(scrollX = TRUE)
      ) %>%
        DT::formatRound(numeric_cols, 2)
    })

    create_table
  })
}
