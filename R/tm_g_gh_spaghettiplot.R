#' Spaghetti Plot
#'
#' This teal module renders the UI and calls the function
#' that creates a spaghetti plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}.
#' E.g. `ADaM` structured laboratory data frame `ADLB`.
#' @param param_var name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data
#' that includes parameter labels.
#' @param idvar name of unique subject id variable.
#' @param xaxis_var single name of variable in analysis data
#' that is used as x-axis in the plot for the respective goshawk function.
#' @param xaxis_var_level vector that can be used to define the factor level of `xaxis_var`.
#' Only use it when `xaxis_var` is character or factor.
#' @param filter_var data constraint variable.
#' @param yaxis_var single name of variable in analysis data that is used as
#' summary variable in the respective `goshawk` function.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
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
#' @inheritParams teal.widgets::standard_layout
#'
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return \code{shiny} object
#'
#' @export
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'
#'   # original ARM value = dose value
#'   arm_mapping <- list(
#'     "A: Drug X" = "150mg QD",
#'     "B: Placebo" = "Placebo",
#'     "C: Combination" = "Combination"
#'   )
#'   set.seed(1)
#'   ADSL <- goshawk::rADSL
#'   ADLB <- goshawk::rADLB
#'   var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'   ADLB <- ADLB %>%
#'     dplyr::mutate(
#'       AVISITCD = dplyr::case_when(
#'         AVISIT == "SCREENING" ~ "SCR",
#'         AVISIT == "BASELINE" ~ "BL",
#'         grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'         TRUE ~ as.character(NA)
#'       ),
#'       AVISITCDN = dplyr::case_when(
#'         AVISITCD == "SCR" ~ -2,
#'         AVISITCD == "BL" ~ 0,
#'         grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'         TRUE ~ as.numeric(NA)
#'       ),
#'       AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'       TRTORD = dplyr::case_when(
#'         ARMCD == "ARM C" ~ 1,
#'         ARMCD == "ARM B" ~ 2,
#'         ARMCD == "ARM A" ~ 3
#'       ),
#'       ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'       ARM = factor(ARM) %>% reorder(TRTORD),
#'       ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'       ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'       ANRLO = 30,
#'       ANRHI = 75
#'     ) %>%
#'     dplyr::rowwise() %>%
#'     dplyr::group_by(PARAMCD) %>%
#'     dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'     )) %>%
#'     dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'     )) %>%
#'     ungroup()
#'   attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#'   attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'   attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#'   # add LLOQ and ULOQ variables
#'   ALB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#'   ADLB <- dplyr::left_join(ADLB, ALB_LOQS, by = "PARAM")
#' })
#'
#' datanames <- c("ADSL", "ADLB")
#' datanames(data) <- datanames
#' join_keys(data) <- default_cdisc_join_keys[datanames]
#'
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     teal.goshawk::tm_g_gh_spaghettiplot(
#'       label = "Spaghetti Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       idvar = "USUBJID",
#'       xaxis_var = choices_selected(c("Analysis Visit Code" = "AVISITCD"), "AVISITCD"),
#'       yaxis_var = choices_selected(c("AVAL", "CHG", "PCHG"), "AVAL"),
#'       filter_var = choices_selected(
#'         c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE"),
#'         "NONE"
#'       ),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       color_comb = "#'39ff14",
#'       man_color = c(
#'         "Combination" = "#'000000",
#'         "Placebo" = "#'fce300",
#'         "150mg QD" = "#'5a2f5f"
#'       ),
#'       hline_arb = c(60, 50),
#'       hline_arb_color = c("grey", "red"),
#'       hline_arb_label = c("default A", "default B"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("pink", "brown", "purple", "black"),
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_spaghettiplot <- function(label,
                                  dataname,
                                  param_var,
                                  param,
                                  param_var_label = "PARAM",
                                  idvar,
                                  xaxis_var,
                                  yaxis_var,
                                  xaxis_var_level = NULL,
                                  filter_var = yaxis_var,
                                  trt_group,
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
                                  hline_arb = numeric(0),
                                  hline_arb_color = "red",
                                  hline_arb_label = "Horizontal line",
                                  hline_vars = character(0),
                                  hline_vars_colors = "green",
                                  hline_vars_labels = hline_vars,
                                  pre_output = NULL,
                                  post_output = NULL) {
  logger::log_info("Initializing tm_g_gh_spaghettiplot")
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
    .var.name = "plot_width"
  )
  checkmate::assert_flag(free_x)

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_spaghettiplot,
    server_args = list(
      dataname = dataname,
      idvar = idvar,
      param_var = param_var,
      trt_group = trt_group,
      xaxis_var_level = xaxis_var_level,
      trt_group_level = trt_group_level,
      man_color = man_color,
      color_comb = color_comb,
      param_var_label = param_var_label,
      xtick = xtick,
      xlabel = xlabel,
      plot_height = plot_height,
      plot_width = plot_width,
      hline_vars_colors = hline_vars_colors,
      hline_vars_labels = hline_vars_labels
    ),
    ui = g_ui_spaghettiplot,
    ui_args = args,
    datanames = dataname
  )
}

g_ui_spaghettiplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  shiny::tagList(
    include_css_files("custom"),
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
          # xparam and yparam are identical, so we only show the user one
          xparam_choices = a$param$choices, xparam_selected = a$param$selected, xparam_label = "Select a Biomarker",
          xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected,
          ychoices = a$yaxis_var$choices, yselected = a$yaxis_var$selected
        ),
        radioButtons(
          ns("group_stats"),
          "Group Statistics",
          c("None" = "NONE", "Mean" = "MEAN", "Median" = "MEDIAN"),
          inline = TRUE
        ),
        templ_ui_constraint(ns), # required by constr_anl_q
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
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            title = "Plot Aesthetic Settings",
            div(
              toggle_slider_ui(
                ns("yrange_scale"),
                label = "Y-Axis Range Zoom",
                min = -1000000,
                max = 1000000,
                value = c(-1000000, 1000000)
              ),
              tags$div(
                class = "flex flex-wrap items-center",
                tags$div(
                  class = "mr-1",
                  tags$span(tags$strong("Number of Plots Per Row:"))
                ),
                tags$div(
                  class = "w-65px",
                  numericInput(ns("facet_ncol"), "", a$facet_ncol, min = 1)
                )
              )
            ),
            checkboxInput(ns("free_x"), "Free X-Axis Scales", a$free_x),
            checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
            teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
            teal.widgets::optionalSliderInputValMinMax(
              ns("alpha"),
              "Line Alpha",
              a$alpha,
              value_min_max = c(0.8, 0.0, 1.0), step = 0.1, ticks = FALSE
            )
          )
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      ),
      pre_output = a$pre_output,
      post_output = a$post_output
    )
  )
}



srv_g_spaghettiplot <- function(id,
                                data,
                                reporter,
                                filter_panel_api,
                                dataname,
                                idvar,
                                param_var,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "tdata")

  moduleServer(id, function(input, output, session) {
    # reused in all modules
    anl_q_output <- constr_anl_q(
      session, input, data, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 1
    )

    anl_q <- anl_q_output()$value

    # update sliders for axes taking constraints into account
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "xaxis_param", anl_q)
    keep_data_const_opts_updated(session, input, anl_q, "xaxis_param")

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      iv$add_rule("xaxis_param", shinyvalidate::sv_required("Please select a biomarker"))
      iv$add_rule("trt_group", shinyvalidate::sv_required("Please select a treatment variable"))
      iv$add_rule("xaxis_var", shinyvalidate::sv_required("Please select an X-Axis variable"))
      iv$add_rule("yaxis_var", shinyvalidate::sv_required("Please select a Y-Axis variable"))
      iv$add_rule("facet_ncol", plots_per_row_validate_rules())

      iv$add_validator(horizontal_line()$iv_r())
      iv$add_validator(anl_q_output()$iv_r())
      iv$enable()
      iv
    })


    plot_q <- reactive({
      teal::validate_inputs(iv_r())
      req(anl_q())
      # nolint start
      ylim <- yrange_slider$state()$value
      facet_ncol <- input$facet_ncol
      facet_scales <- ifelse(input$free_x, "free_x", "fixed")

      rotate_xlab <- input$rotate_xlab
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      group_stats <- input$group_stats
      font_size <- input$font_size
      alpha <- input$alpha
      validate(need(input$trt_group, "Please select a treatment variable"))
      trt_group <- input$trt_group

      # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_q()) and some inputs
      param <- input$xaxis_param
      xaxis_var <- input$xaxis_var
      yaxis_var <- input$yaxis_var
      hline_vars <- input$hline_vars
      # nolint end

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

      teal.code::eval_code(
        object = private_qenv,
        code = bquote({
          p <- goshawk::g_spaghettiplot(
            data = ANL,
            subj_id = .(idvar),
            biomarker_var = .(param_var),
            biomarker_var_label = .(param_var_label),
            biomarker = .(param),
            value_var = .(yaxis_var),
            trt_group = .(trt_group),
            trt_group_level = .(trt_group_level),
            time = .(xaxis_var),
            time_level = .(xaxis_var_level),
            color_manual = .(man_color),
            color_comb = .(color_comb),
            ylim = .(ylim),
            facet_ncol = .(facet_ncol),
            facet_scales = .(facet_scales),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            xtick = xtick,
            xlabel = xlabel,
            rotate_xlab = .(rotate_xlab),
            font_size = .(font_size),
            alpha = .(alpha),
            group_stats = .(group_stats),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars)])
          )
          print(p)
        })
      )
    })

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

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template_goshawk(
          title = "Spaghetti Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api,
          constraint_list = list(
            constraint_var = input$constraint_var,
            constraint_range_min = input$constraint_range_min,
            constraint_range_max = input$constraint_range_max
          )
        )
        card$append_text("Spaghetti Plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(plot_q()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###

    output$brush_data <- DT::renderDataTable({
      plot_brush <- plot_data$brush()

      ANL <- isolate(anl_q()$ANL) # nolint
      validate_has_data(ANL, 1)

      xvar <- isolate(input$xaxis_var)
      yvar <- isolate(input$yaxis_var)
      trt_group <- isolate(input$trt_group)

      req(all(c(xvar, yvar) %in% names(ANL)))

      df <- teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL, "USUBJID", dplyr::all_of(trt_group), "PARAMCD",
          dplyr::all_of(c(xvar, yvar)), "LOQFL"
        ),
        plot_brush
      )
      df <- df[order(df$PARAMCD, df[[trt_group]], df$USUBJID, df[[xvar]]), ]
      numeric_cols <- names(dplyr::select_if(df, is.numeric))

      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(plot_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(plot_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(plot_q())),
      title = "Show R Code for Spaghetti Plot"
    )
  })
}
