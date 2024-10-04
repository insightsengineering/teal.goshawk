#' Box Plot
#'
#' This teal module renders the UI and calls the functions that create a box plot and accompanying
#' summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of \code{\link[teal]{init}}. E.g. `ADaM` structured
#'  laboratory data frame `ALB`.
#' @param param_var name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param list of biomarkers of interest.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. `AVAL`. When not provided,
#' it defaults to `choices_selected(c("AVAL", "CHG"), "AVAL")`.
#' @param xaxis_var variable to categorize the x-axis. When not provided, it defaults to
#' `choices_selected("AVISITCD", "AVISITCD")`.
#' @param facet_var variable to facet the plots by. When not provided, it defaults to
#' `choices_selected(c("ARM", "ACTARM"), "ARM")`.
#' @param trt_group  \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected
#'  option for variable names representing treatment group e.g. `ARM`.
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
#'
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#' data <- teal_data()
#' data <- within(data, {
#'   library(dplyr)
#'   library(nestcolor)
#'   library(stringr)
#'
#'   # use non-exported function from goshawk
#'   h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")
#'
#'   # original ARM value = dose value
#'   arm_mapping <- list(
#'     "A: Drug X" = "150mg QD",
#'     "B: Placebo" = "Placebo",
#'     "C: Combination" = "Combination"
#'   )
#'   set.seed(1)
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
#'   attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'   attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#'   attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'   attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#'   # add LLOQ and ULOQ variables
#'   ALB_LOQS <- h_identify_loq_values(ADLB, "LOQFL")
#'   ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
#' })
#'
#' datanames <- c("ADSL", "ADLB")
#' datanames(data) <- datanames
#'
#' join_keys(data) <- default_cdisc_join_keys[datanames]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_gh_boxplot(
#'       label = "Box Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
#'       xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
#'       facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
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
                            alpha = c(0.8, 0.0, 1.0),
                            pre_output = NULL,
                            post_output = NULL) {
  message("Initializing tm_g_gh_boxplot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(param_var)
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(facet_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_int(facet_ncol, null.ok = TRUE)
  checkmate::assert_flag(loq_legend)
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(dot_size, len = 3)
  checkmate::assert_numeric(alpha, len = 3)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
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
    server = srv_g_boxplot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      trt_group = trt_group,
      color_manual = color_manual,
      shape_manual = shape_manual,
      plot_height = plot_height,
      plot_width = plot_width,
      hline_vars_colors = hline_vars_colors,
      hline_vars_labels = hline_vars_labels,
      module_args = args
    ),
    ui = ui_g_boxplot,
    ui_args = args
  )
}

ui_g_boxplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = tags$div(
      fluidRow(
        teal.widgets::plot_with_settings_ui(id = ns("boxplot"))
      ),
      fluidRow(column(
        width = 12,
        tags$br(), tags$hr(),
        tags$h4("Selected Data Points"),
        DT::dataTableOutput(ns("brush_data"))
      )),
      fluidRow(column(
        width = 12,
        tags$br(), tags$hr(),
        tags$h4("Descriptive Statistics"),
        DT::dataTableOutput(ns("table_ui"))
      ))
    ),
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
      templ_ui_constraint(ns, label = "Data Constraint"), # required by constr_anl_q
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
          toggle_slider_ui(
            ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", a$loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab)
        ),
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Dot Alpha", a$alpha, ticks = FALSE)
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


srv_g_boxplot <- function(id,
                          data,
                          reporter,
                          filter_panel_api,
                          dataname,
                          param_var,
                          trt_group,
                          color_manual,
                          shape_manual,
                          plot_height,
                          plot_width,
                          hline_vars_colors,
                          hline_vars_labels,
                          module_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    output$axis_selections <- renderUI({
      env <- shiny::isolate(as.list(data()@env))
      resolved_x <- teal.transform::resolve_delayed(module_args$xaxis_var, env)
      resolved_y <- teal.transform::resolve_delayed(module_args$yaxis_var, env)
      resolved_param <- teal.transform::resolve_delayed(module_args$param, env)
      resolved_facet_var <- teal.transform::resolve_delayed(module_args$facet_var, env)

      templ_ui_params_vars(
        session$ns,
        xparam_choices = resolved_param$choices,
        xparam_selected = resolved_param$selected,
        xparam_label = module_args$"Select a Biomarker",
        xchoices = resolved_x$choices,
        xselected = resolved_x$selected,

        ychoices = resolved_y$choices,
        yselected = resolved_y$selected,

        facet_choices = resolved_facet_var$choices,
        facet_selected = resolved_facet_var$selected
      )
    })
    # reused in all modules
    anl_q_output <- constr_anl_q(
      session, input, data, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 2
    )

    anl_q <- anl_q_output()$value

    # update sliders for axes taking constraints into account
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(
      session,
      input,
      update_slider_fcn = yrange_slider$update_state,
      id_var = "yaxis_var",
      id_param_var = "xaxis_param",
      reactive_ANL = anl_q
    )
    keep_data_const_opts_updated(session, input, anl_q, "xaxis_param")

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    trt_group <- reactive({
      input$trt_group
    })

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()

      iv$add_rule("xaxis_param", shinyvalidate::sv_required("Please select a biomarker"))
      iv$add_rule("trt_group", shinyvalidate::sv_required("Please select a treatment variable"))
      iv$add_rule("xaxis_var", shinyvalidate::sv_required("Please select an X-Axis variable"))
      iv$add_rule("xaxis_var", ~ if ((.) %in% c("ACTARM", "ARM") && isTRUE((.) != trt_group())) {
        sprintf("You can not choose %s as x-axis variable for treatment variable %s.", (.), trt_group())
      })
      iv$add_rule("yaxis_var", shinyvalidate::sv_required("Please select a Y-Axis variable"))

      iv$add_rule("facet_var", shinyvalidate::sv_optional())
      iv$add_rule("facet_var", ~ if ((.) %in% c("ACTARM", "ARM") && isTRUE((.) != trt_group())) {
        sprintf("You can not choose %s as faceting variable for treatment variable %s.", (.), trt_group())
      })

      iv_facet <- shinyvalidate::InputValidator$new()
      iv_facet$condition(~ !is.null(input$facet_var))
      iv_facet$add_rule("facet_ncol", plots_per_row_validate_rules(required = FALSE))
      iv$add_validator(iv_facet)

      iv$add_validator(horizontal_line()$iv_r())
      iv$add_validator(anl_q_output()$iv_r())
      iv$enable()
      iv
    })

    create_plot <- debounce(reactive({
      teal::validate_inputs(iv_r())

      req(anl_q())
      # nolint start
      param <- input$xaxis_param
      yaxis <- input$yaxis_var
      xaxis <- input$xaxis_var
      facet_var <- `if`(is.null(input$facet_var), "None", input$facet_var)
      ylim <- yrange_slider$state()$value
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
      trt_group <- input$trt_group
      # nolint end

      validate_has_variable(
        anl_q()$ANL,
        yaxis,
        sprintf("Variable %s is not available in data %s", yaxis, dataname)
      )
      validate_has_variable(
        anl_q()$ANL,
        xaxis,
        sprintf("Variable %s is not available in data %s", xaxis, dataname)
      )

      if (!facet_var == "None") {
        validate_has_variable(
          anl_q()$ANL,
          facet_var,
          sprintf("Variable %s is not available in data %s", facet_var, dataname)
        )
      }

      anl_q()$qenv %>% teal.code::eval_code(
        code = bquote({
          p <- goshawk::g_boxplot(
            data = ANL,
            biomarker = .(param),
            xaxis_var = .(xaxis),
            yaxis_var = .(yaxis),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars)]),
            facet_ncol = .(facet_ncol),
            loq_legend = .(loq_legend),
            rotate_xlab = .(rotate_xlab),
            trt_group = .(trt_group),
            ylim = .(ylim),
            color_manual = .(color_manual),
            shape_manual = .(shape_manual),
            facet_var = .(facet_var),
            alpha = .(alpha),
            dot_size = .(dot_size),
            font_size = .(font_size),
            unit = .("AVALU")
          )
        })
      )
    }), 800)

    create_table <- debounce(reactive({
      req(iv_r()$is_valid())
      req(anl_q())
      param <- input$xaxis_param
      xaxis_var <- input$yaxis_var # nolint
      font_size <- input$font_size
      trt_group <- input$trt_group
      facet_var <- input$facet_var

      anl_q()$qenv %>% teal.code::eval_code(
        code = bquote({
          tbl <- goshawk::t_summarytable(
            data = ANL,
            trt_group = .(trt_group),
            param_var = .(param_var),
            param = .(param),
            xaxis_var = .(xaxis_var),
            facet_var = .(facet_var)
          )
        })
      )
    }), 800)

    plot_r <- reactive({
      create_plot()[["p"]]
    })

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

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        constraint_description <- paste(
          "\nFacet By:",
          if (length(input$facet_var) != 0) input$facet_var else "None",
          "\nSelect an X-axis Variable:",
          input$xaxis_var
        )
        card <- report_card_template_goshawk(
          title = "Box Plot",
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
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = boxplot_data$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(
          teal.code::get_code(
            teal.code::join(create_plot(), create_table())
          )
        )
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###

    # highlight plot area
    reactive_df <- debounce(reactive({
      boxplot_brush <- boxplot_data$brush()

      ANL <- isolate(anl_q()$ANL) %>% droplevels() # nolint
      validate_has_data(ANL, 2)

      xvar <- isolate(input$xaxis_var)
      yvar <- isolate(input$yaxis_var)
      facetv <- isolate(input$facet_var)
      trt_group <- isolate(input$trt_group)

      req(all(c(xvar, yvar, facetv, trt_group) %in% names(ANL)))

      teal.widgets::clean_brushedPoints(
        dplyr::select(
          ANL, "USUBJID", dplyr::all_of(c(trt_group, facetv)),
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

    joined_qenvs <- reactive({
      req(create_plot(), create_table())
      teal.code::join(create_plot(), create_table())
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(joined_qenvs())),
      title = "Show R Code for Boxplot"
    )
  })
}
