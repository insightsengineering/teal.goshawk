#' Spaghetti Plot
#'
#' This teal module renders the UI and calls the function
#' that creates a spaghetti plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}.
#' E.g. `ADaM` structured laboratory data frame `ADLB`.
#' @param param_var `r badge("deprecated")` name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param (`picks` or `choices_selected`) biomarker selected.
#' @param param_var_label single name of variable in analysis data
#' that includes parameter labels.
#' @param idvar name of unique subject id variable.
#' @param xaxis_var (`variables` or `choices_selected`)
#' single name of variable in analysis data that is used as x-axis in the plot.
#' @param xaxis_var_level vector that can be used to define the factor level of `xaxis_var`.
#' Only use it when `xaxis_var` is character or factor.
#' @param filter_var `r badge("deprecated")` data constraint variable.
#' @param yaxis_var (`variables` or `choices_selected`) single name of variable in analysis data that is used as
#' summary variable in the respective `goshawk` function.
#' @param trt_group (`variables` or `choices_selected`) object with available choices and pre-selected option
#' for variable names representing treatment group e.g. `ARM`.
#' @param trt_group_level vector that can be used to define factor
#' level of `trt_group`.
#' @param man_color string vector representing customized colors
#' @param color_comb name or hex value for combined treatment color.
#' @param xtick numeric vector to define the tick values of `x-axis`
#' when x variable is numeric. Default value is `waive()`.
#' @param xlabel vector with same length of `xtick` to define the
#' label of `x-axis` tick values. Default value is `waive()`.
#' @param rotate_xlab `logical(1)` value indicating whether to rotate `x-axis` labels
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param free_x `logical(1)` should scales be `"fixed"` (`FALSE`) of `"free"` (`TRUE`) for `x-axis` in
#' \code{\link[ggplot2]{facet_wrap}} \code{scales} parameter.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size control font size for title, `x-axis`, `y-axis` and legend font.
#' @param dot_size plot dot size.
#' @param group_stats control group mean or median overlay.
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param hline_vars a character vector to name the columns that will define additional horizontal lines.
#' @param hline_vars_colors a character vector naming the colors for the additional horizontal lines.
#' @param hline_vars_labels a character vector naming the labels for the additional horizontal lines that will appear
#'  in the legend.
#' @param alpha (`numeric(3)`) vector to define transparency of plotted points.
#' @inheritParams teal::module
#' @inheritParams teal.widgets::standard_layout
#'
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @inheritSection teal::example_module Reporting
#'
#' @return \code{shiny} object
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
#'       ANRLO = 30,
#'       ANRHI = 75
#'     ) %>%
#'     rowwise() %>%
#'     group_by(PARAMCD) %>%
#'     mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'     )) %>%
#'     mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'     )) %>%
#'     ungroup()
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
#'     tm_g_gh_spaghettiplot(
#'       label = "Spaghetti Plot",
#'       dataname = "ADLB",
#'       param = picks(
#'         variables("PARAMCD", "PARAMCD"),
#'         values(selected = "ALT", multiple = FALSE),
#'         check_dataset = FALSE
#'       ),
#'       idvar = "USUBJID",
#'       xaxis_var = variables(c("AVISITCD", "AVISIT"), "AVISITCD"),
#'       yaxis_var = variables(c("AVAL", "CHG", "PCHG"), "AVAL"),
#'       trt_group = variables(c("ARM", "ACTARM"), "ARM"),
#'       color_comb = "#39ff14",
#'       man_color = c(
#'         "Combination" = "#000000",
#'         "Placebo" = "#fce300",
#'         "150mg QD" = "#5a2f5f"
#'       ),
#'       hline_arb = c(60, 50),
#'       hline_arb_color = c("grey", "red"),
#'       hline_arb_label = c("default A", "default B"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("pink", "brown", "purple", "black")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_spaghettiplot <- function(label,
                                  dataname = "ADLB",
                                  param_var = lifecycle::deprecated(),
                                  param = teal.picks::picks(
                                    teal.picks::variables("PARAMCD", "PARAMCD"),
                                    teal.picks::values(selected = "ALT", multiple = FALSE),
                                    check_dataset = FALSE
                                  ),
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
                                  alpha = c(0.8, 0.0, 1.0),
                                  pre_output = NULL,
                                  post_output = NULL,
                                  transformators = list()) {
  message("Initializing tm_g_gh_spaghettiplot")

  if (lifecycle::is_present(filter_var)) {
    lifecycle::deprecate_warn("0.6.0", "tm_g_gh_spaghettiplot(filter_var)", details = "Variable has been removed.")
  }

  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(param_var_label)
  checkmate::assert_string(idvar)
  checkmate::assert_string(group_stats)

  checkmate::assert_multi_class(param, c("choices_selected", "picks"))
  checkmate::assert_multi_class(xaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(yaxis_var, c("choices_selected", "variables", "picks"))
  checkmate::assert_multi_class(trt_group, c("choices_selected", "variables", "picks"))

  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_flag(free_x)

  checkmate::assert_character(man_color, null.ok = TRUE)
  checkmate::assert_character(color_comb, null.ok = TRUE)
  checkmate::assert_character(xaxis_var_level, null.ok = TRUE)
  checkmate::assert_character(trt_group_level, null.ok = TRUE)
  checkmate::assert_numeric(hline_arb, null.ok = TRUE)
  checkmate::assert_character(hline_arb_color)
  checkmate::assert_character(hline_arb_label)
  checkmate::assert_character(hline_vars)
  checkmate::assert_integerish(facet_ncol, lower = 1, len = 1)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3],
    null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(dot_size, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(alpha, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)
  checkmate::assert_list(transformators, types = "teal_transform_module")

  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)

  if (lifecycle::is_present(param_var)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "tm_g_gh_spaghettiplot(param_var)",
      details = "Please use `teal.picks::picks()` to specify `param` instead of `param_var`."
    )
    checkmate::assert_string(param_var)
    param_var <- teal.picks::variables(param_var, param_var)
  }

  if (inherits(param, "choices_selected")) {
    stopifnot("param_var is necessary when providing param with `choices_selected()`. Consider moving to `param = teal.picks::picks(...)`" = inherits(param_var, "variables")) # nolint: line_length_linter.
    param <- migrate_choices_selected_to_values(param)
    param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param_var, param)
  } else {
    param <- create_picks_helper(teal.picks::datasets(dataname, dataname), param)
  }

  xaxis_var <- migrate_choices_selected_to_variables(xaxis_var)
  yaxis_var <- migrate_choices_selected_to_variables(yaxis_var)
  trt_group <- migrate_choices_selected_to_variables(trt_group)

  teal.picks::assert_last_level(param, "values")

  xaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), xaxis_var)
  yaxis_var <- create_picks_helper(teal.picks::datasets(dataname, dataname), yaxis_var)
  trt_group <- create_picks_helper(teal.picks::datasets(dataname, dataname), trt_group)

  param <- force_pick_selection(param, which = "values")
  trt_group <- force_pick_selection(trt_group, which = "variables")
  xaxis_var <- force_pick_selection(xaxis_var, which = "variables")
  yaxis_var <- force_pick_selection(yaxis_var, which = "variables")

  args <- as.list(environment())

  module(
    label = label,
    datanames = .picks_datanames(param, xaxis_var, yaxis_var, trt_group),
    server = srv_g_spaghettiplot,
    server_args = args[names(args) %in% names(formals(srv_g_spaghettiplot))],
    ui = g_ui_spaghettiplot,
    ui_args = args[names(args) %in% names(formals(g_ui_spaghettiplot))],
    transformators = transformators
  )
}

g_ui_spaghettiplot <- function(id,
                               dataname,
                               param,
                               xaxis_var,
                               yaxis_var,
                               trt_group,
                               facet_ncol,
                               free_x,
                               rotate_xlab,
                               hline_arb,
                               hline_arb_color,
                               hline_arb_label,
                               hline_vars,
                               font_size,
                               dot_size,
                               alpha,
                               group_stats,
                               pre_output,
                               post_output) {
  ns <- NS(id)

  tags$div(
    teal.widgets::standard_layout(
      output = templ_ui_output_datatable(ns),
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
        radioButtons(
          ns("group_stats"),
          "Group Statistics",
          c("None" = "NONE", "Mean" = "MEAN", "Median" = "MEDIAN"),
          inline = TRUE,
          selected = group_stats
        ),
        templ_ui_constraint(ns),
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
        bslib::accordion(
          bslib::accordion_panel(
            title = "Plot Aesthetic Settings",
            tags$div(
              toggle_slider_ui(
                ns("yrange_scale"),
                label = "Y-Axis Range Zoom"
              ),
              tags$div(
                class = "flex flex-wrap items-center",
                tags$div(
                  class = "mr-1",
                  tags$span(tags$strong("Number of Plots Per Row:"))
                ),
                tags$div(
                  class = "w-65px",
                  numericInput(ns("facet_ncol"), "", facet_ncol, min = 1)
                )
              )
            ),
            checkboxInput(ns("free_x"), "Free X-Axis Scales", free_x),
            checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", rotate_xlab),
            teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", font_size, ticks = FALSE),
            teal.widgets::optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", dot_size, ticks = FALSE),
            teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Line Alpha", alpha, ticks = FALSE)
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}


srv_g_spaghettiplot <- function(id,
                                data,
                                dataname,
                                idvar,
                                param,
                                xaxis_var,
                                yaxis_var,
                                trt_group,
                                man_color,
                                color_comb,
                                xaxis_var_level,
                                trt_group_level,
                                param_var_label,
                                xtick,
                                xlabel,
                                plot_height,
                                plot_width,
                                hline_vars_colors,
                                hline_vars_labels) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")

    selectors <- teal.picks::picks_srv(
      id = "",
      picks = list(
        xaxis_param = param,
        xaxis_var = xaxis_var,
        yaxis_var = yaxis_var,
        trt_group = trt_group
      ),
      data = data
    )

    param_sel <- reactive(selectors$xaxis_param()$values$selected)
    xaxis_var_sel <- reactive(selectors$xaxis_var()$variables$selected)
    yaxis_var_sel <- reactive(selectors$yaxis_var()$variables$selected)
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
          condition = length(input$facet_ncol) != 0 && input$facet_ncol > 0 && as.numeric(input$facet_ncol) %% 1 == 0,
          message = "Please select a facet column integer that is greater than 0"
        )
      )
      data_with_card()
    })

    anl_q_output <- constr_anl_q(
      session, input, validated_q, dataname,
      param_r = param_sel, param_var_r = param_var_sel, trt_group_r = trt_group_sel, min_rows = 1
    )

    anl_q <- anl_q_output()$value

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


    plot_q <- debounce(reactive({
      teal::validate_inputs(iv_r())
      req(anl_q())

      validate( # Validation must occur after anl_constraint() has valid data
        teal::need_input(
          "yrange_scale",
          !is.null(yrange_slider$value) &&
            length(yrange_slider$value) == 2 &&
            yrange_slider$value[1] < yrange_slider$value[2],
          "Y-Axis Range Zoom: Invalid range"
        )
      )

      ylim <- yrange_slider$value
      facet_ncol_val <- input$facet_ncol
      facet_scales <- ifelse(input$free_x, "free_x", "fixed")

      rotate_xlab_val <- input$rotate_xlab
      hline_arb_val <- horizontal_line()$line_arb
      hline_arb_label_val <- horizontal_line()$line_arb_label
      hline_arb_color_val <- horizontal_line()$line_arb_color
      group_stats_val <- input$group_stats
      font_size_val <- input$font_size
      dot_size_val <- input$dot_size
      alpha_val <- input$alpha
      hline_vars_val <- input$hline_vars

      private_qenv <- anl_q()$qenv

      # this code is needed to make sure the waiver attribute
      # of ggplot2::waiver is correctly passed to goshawk's spaghettiplot
      if (!methods::is(xtick, "waiver")) {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = bquote(xtick <- .(xtick))
        )
      } else {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = quote(xtick <- ggplot2::waiver())
        )
      }

      if (!methods::is(xlabel, "waiver")) {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = bquote(xlabel <- .(xlabel))
        )
      } else {
        private_qenv <- teal.code::eval_code(
          object = private_qenv,
          code = quote(xlabel <- ggplot2::waiver())
        )
      }

      obj <- private_qenv
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("### Plot")
        )

      teal.code::eval_code(
        object = obj,
        code = bquote({
          p <- goshawk::g_spaghettiplot(
            data = ANL,
            subj_id = .(idvar),
            biomarker_var = .(param_var_sel()),
            biomarker_var_label = .(param_var_label),
            biomarker = .(param_sel()),
            value_var = .(yaxis_var_sel()),
            trt_group = .(trt_group_sel()),
            trt_group_level = .(trt_group_level),
            time = .(xaxis_var_sel()),
            time_level = .(xaxis_var_level),
            color_manual = .(man_color),
            color_comb = .(color_comb),
            ylim = .(ylim),
            facet_ncol = .(facet_ncol_val),
            facet_scales = .(facet_scales),
            hline_arb = .(hline_arb_val),
            hline_arb_label = .(hline_arb_label_val),
            hline_arb_color = .(hline_arb_color_val),
            xtick = xtick,
            xlabel = xlabel,
            rotate_xlab = .(rotate_xlab_val),
            font_size = .(font_size_val),
            dot_size = .(dot_size_val),
            alpha = .(alpha_val),
            group_stats = .(group_stats_val),
            hline_vars = .(hline_vars_val),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars_val)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars_val)])
          )
          p
        })
      )
    }), 800)

    plot_r <- reactive({
      plot_q()[["p"]]
    })

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )


    reactive_df <- debounce(reactive({
      plot_brush <- plot_data$brush()

      ANL <- isolate(anl_q()$ANL)
      validate_has_data(ANL, 1)

      xvar <- isolate(xaxis_var_sel())
      yvar <- isolate(yaxis_var_sel())
      trt_group <- isolate(trt_group_sel())

      req(all(c(xvar, yvar) %in% names(ANL)))

      df <- teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL, "USUBJID", dplyr::all_of(trt_group), "PARAMCD",
          dplyr::all_of(c(xvar, yvar)), "LOQFL"
        ),
        plot_brush
      )
      df[order(df$PARAMCD, df[[trt_group]], df$USUBJID, df[[xvar]]), ]
    }), 800)

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
