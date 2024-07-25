#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#'  `tm_g_gh_scatterplot` is deprecated. Please use [tm_g_gh_correlationplot]
#'   instead.
#'
#' @inheritParams teal.widgets::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#' laboratory data frame \code{ADLB}.
#' @param param_var name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on `x-axis` e.g. \code{BASE}.
#' @param yaxis_var name of variable containing biomarker results displayed on `y-axis` e.g. \code{AVAL}.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. `ARM`.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param trt_facet facet by treatment group \code{trt_group}.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param rotate_xlab 45 degree rotation of `x-axis` values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, `x-axis` label, `y-axis` label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#'
#' @export
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(stringr)
#'
#'   # original ARM value = dose value
#'   arm_mapping <- list(
#'     "A: Drug X" = "150mg QD",
#'     "B: Placebo" = "Placebo",
#'     "C: Combination" = "Combination"
#'   )
#'
#'   ADSL <- rADSL
#'   ADLB <- rADLB
#'   var_labels <- lapply(ADLB, function(x) attributes(x)$label)
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
#'       ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'       ARM = factor(ARM) %>% reorder(TRTORD),
#'       ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'       ACTARM = factor(ACTARM) %>% reorder(TRTORD)
#'     )
#'   attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#' })
#'
#' datanames <- c("ADSL", "ADLB")
#' datanames(data) <- datanames
#' join_keys(data) <- default_cdisc_join_keys[datanames]
#'
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_scatterplot(
#'       label = "Scatter Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       color_manual = c(
#'         "150mg QD" = "#000000",
#'         "Placebo" = "#3498DB",
#'         "Combination" = "#E74C3C"
#'       ),
#'       shape_manual = c("N" = 1, "Y" = 2, "NA" = 0),
#'       plot_height = c(500, 200, 2000),
#'       facet_ncol = 2,
#'       trt_facet = FALSE,
#'       reg_line = FALSE,
#'       font_size = c(12, 8, 20),
#'       dot_size = c(1, 1, 12),
#'       reg_text_size = c(3, 3, 10)
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_scatterplot <- function(label,
                                dataname,
                                param_var,
                                param,
                                xaxis_var,
                                yaxis_var,
                                trt_group,
                                color_manual = NULL,
                                shape_manual = NULL,
                                facet_ncol = 2,
                                trt_facet = FALSE,
                                reg_line = FALSE,
                                rotate_xlab = FALSE,
                                hline = NULL,
                                vline = NULL,
                                plot_height = c(500, 200, 2000),
                                plot_width = NULL,
                                font_size = c(12, 8, 20),
                                dot_size = c(1, 1, 12),
                                reg_text_size = c(3, 3, 10),
                                pre_output = NULL,
                                post_output = NULL) {
  lifecycle::deprecate_soft(
    when = "0.1.15",
    what = "tm_g_gh_scatterplot()",
    details = "You should use teal.goshawk::tm_g_gh_correlationplot instead of teal.goshawk::tm_g_gh_scatterplot"
  )

  message("Initializing tm_g_gh_scatterplot")
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_flag(trt_facet)
  checkmate::assert_flag(reg_line)
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  args <- as.list(environment())

  module(
    label = label,
    datanames = dataname,
    server = srv_g_scatterplot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      trt_group = trt_group,
      trt_facet = trt_facet,
      color_manual = color_manual,
      shape_manual = shape_manual,
      plot_height = plot_height,
      plot_width = plot_width,
      module_args = args
    ),
    ui = ui_g_scatterplot,
    ui_args = args
  )
}

ui_g_scatterplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      templ_ui_dataname(a$dataname),
      teal.widgets::optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = get_choices(a$trt_group$choices),
        selected = a$trt_group$selected,
        multiple = FALSE
      ),
      uiOutput(ns("axis_selections")),
      templ_ui_constraint(ns), # required by constr_anl_q
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(ns("xrange_scale"),
            label = "X-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          toggle_slider_ui(ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("trt_facet"), "Treatment Variable Faceting", a$trt_facet),
          checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline),
          numericInput(ns("vline"), "Add a vertical line:", a$vline)
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_scatterplot <- function(id,
                              data,
                              reporter,
                              filter_panel_api,
                              dataname,
                              param_var,
                              trt_group,
                              trt_facet,
                              color_manual,
                              shape_manual,
                              plot_height,
                              plot_width,
                              module_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.goshawk")
    output$axis_selections <- renderUI({
      env <- shiny::isolate(as.list(data()@env))
      resolved_x <- teal.transform::resolve_delayed(module_args$xaxis_var, env)
      resolved_y <- teal.transform::resolve_delayed(module_args$yaxis_var, env)
      resolved_param <- teal.transform::resolve_delayed(module_args$param, env)
      templ_ui_params_vars(
        session$ns,
        # xparam and yparam are identical, so we only show the user one
        xparam_choices = resolved_param$choices,
        xparam_selected = resolved_param$selected,
        xparam_label = "Select a Biomarker",
        xchoices = resolved_x$choices,
        xselected = resolved_x$selected,
        ychoices = resolved_y$choices,
        yselected = resolved_y$selected
      )
    })

    # reused in all modules
    anl_q_output <- constr_anl_q(
      session, input, data, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 1
    )

    anl_q <- anl_q_output()$value

    # update sliders for axes taking constraints into account
    xrange_slider <- toggle_slider_server("xrange_scale")
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(session, input, xrange_slider$update_state, "xaxis_var", "xaxis_param", anl_q)
    keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "xaxis_param", anl_q)
    keep_data_const_opts_updated(session, input, anl_q, "xaxis_param")

    # plot
    plot_q <- reactive({
      req(anl_q())
      # nolint start
      xlim <- xrange_slider$state()$value
      ylim <- yrange_slider$state()$value
      facet_ncol <- input$facet_ncol
      validate(need(
        is.na(facet_ncol) || (as.numeric(facet_ncol) > 0 && as.numeric(facet_ncol) %% 1 == 0),
        "Number of plots per row must be a positive integer"
      ))
      reg_line <- input$reg_line
      font_size <- input$font_size
      dot_size <- input$dot_size
      reg_text_size <- input$reg_text_size
      rotate_xlab <- input$rotate_xlab
      hline <- input$hline
      vline <- input$vline
      trt_group <- input$trt_group
      facet <- input$trt_facet
      validate(need(trt_group, "Please select a treatment variable"))

      # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_q()) and some inputs
      validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
      validate(need(input$yaxis_var, "Please select a Y-Axis Variable"))
      param <- input$xaxis_param
      xaxis <- input$xaxis_var
      yaxis <- input$yaxis_var

      # nolint end
      teal.code::eval_code(
        object = anl_q()$qenv,
        code = bquote({
          # re-establish treatment variable label
          p <- goshawk::g_scatterplot(
            data = ANL,
            param_var = .(param_var),
            param = .(param),
            xaxis_var = .(xaxis),
            yaxis_var = .(yaxis),
            trt_group = .(trt_group),
            xlim = .(xlim),
            ylim = .(ylim),
            color_manual = .(color_manual),
            shape_manual = .(shape_manual),
            facet_ncol = .(facet_ncol),
            facet = .(facet),
            facet_var = .(trt_group),
            reg_line = .(reg_line),
            font_size = .(font_size),
            dot_size = .(dot_size),
            reg_text_size = .(reg_text_size),
            rotate_xlab = .(rotate_xlab),
            hline = .(`if`(is.na(hline), NULL, as.numeric(hline))),
            vline = .(`if`(is.na(vline), NULL, as.numeric(vline)))
          )
          print(p)
        })
      )
    })

    plot_r <- reactive(plot_q()[["p"]])

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    code <- reactive(teal.code::get_code(plot_q()))

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        constraint_description <- paste(
          "\nTreatment Variable Faceting:",
          input$trt_facet,
          "\nRegression Line:",
          input$reg_line
        )
        card <- report_card_template_goshawk(
          title = "Scatter Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api,
          constraint_list = list(
            constraint_var = input$constraint_var,
            constraint_range_min = input$constraint_range_min,
            constraint_range_max = input$constraint_range_max
          ),
          constraint_description = constraint_description,
          style = "verbatim"
        )
        card$append_text("Scatter Plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(code())
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###

    # highlight plot area
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
          ANL, "USUBJID", dplyr::all_of(trt_group), "AVISITCD", "PARAMCD",
          dplyr::all_of(c(xvar, yvar)), "LOQFL"
        ),
        plot_brush
      )

      numeric_cols <- names(dplyr::select_if(df, is.numeric))

      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(code()),
      title = "Show R Code for Scatterplot"
    )
  })
}
