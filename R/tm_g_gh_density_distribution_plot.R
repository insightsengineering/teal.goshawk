#' Density Distribution Plot
#'
#' This teal module renders the UI and calls the functions that create a density distribution plot
#' and an accompanying summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param color_comb name or hex value for combined treatment color.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param comb_line display combined treatment line toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#'
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams tm_g_gh_scatterplot
#'
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details None
#'
#' @export
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(scda)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   dplyr::mutate(
#'     AVISITCD = dplyr::case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)
#'     ),
#'     AVISITCDN = dplyr::case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)
#'     ),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = dplyr::case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3
#'     ),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD),
#'     ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'     ACTARM = factor(ACTARM) %>% reorder(TRTORD)
#'   )
#'
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "ADLB <- synthetic_cdisc_data(\"latest\")$adlb
#'               var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'               ADLB <- ADLB %>%
#'                 dplyr::mutate(AVISITCD = dplyr::case_when(
#'                     AVISIT == 'SCREENING' ~ 'SCR',
#'                     AVISIT == 'BASELINE' ~ 'BL',
#'                     grepl('WEEK', AVISIT) ~
#'                       paste('W', stringr::str_extract(AVISIT, '(?<=(WEEK ))[0-9]+')),
#'                     TRUE ~ as.character(NA)),
#'                   AVISITCDN = dplyr::case_when(
#'                     AVISITCD == 'SCR' ~ -2,
#'                     AVISITCD == 'BL' ~ 0,
#'                     grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'                     TRUE ~ as.numeric(NA)),
#'                   AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'                   TRTORD = dplyr::case_when(
#'                     ARMCD == 'ARM C' ~ 1,
#'                     ARMCD == 'ARM B' ~ 2,
#'                    ARMCD == 'ARM A' ~ 3),
#'                  ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'                  ARM = factor(ARM) %>% reorder(TRTORD),
#'                  ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'                  ACTARM = factor(ACTARM) %>% reorder(TRTORD))
#'                attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'                attr(ADLB[['ACTARM']], 'label') <- var_labels[['ACTARM']]",
#'       vars = list(arm_mapping = arm_mapping)
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_gh_density_distribution_plot(
#'       label = "Density Distribution Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_gh_density_distribution_plot <- function(label, # nolint
                                              dataname,
                                              param_var,
                                              param,
                                              xaxis_var,
                                              trt_group,
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
                                              post_output = NULL) {
  logger::log_info("Initializing tm_g_gh_density_distribution_plot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(param_var)
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(line_size, len = 3)
  checkmate::assert_int(facet_ncol)
  checkmate::assert_flag(comb_line)
  checkmate::assert_flag(rotate_xlab)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
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
    filters = dataname,
    server = srv_g_density_distribution_plot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      param = param,
      trt_group = trt_group,
      color_manual = color_manual,
      color_comb = color_comb,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_g_density_distribution_plot,
    ui_args = args
  )
}

ui_g_density_distribution_plot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = div(
      fluidRow(
        teal.widgets::plot_with_settings_ui(id = ns("plot"))
      ),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Descriptive Statistics"),
        DT::dataTableOutput(ns("table_ui"))
      ))
    ),
    encoding = div(
      ### Reporter
      shiny::tags$div(
        teal.reporter::add_card_button_ui(ns("addReportCard")),
        teal.reporter::download_report_button_ui(ns("downloadButton")),
        teal.reporter::reset_report_button_ui(ns("resetButton"))
      ),
      shiny::tags$br(),
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
        xparam_choices = a$param$choices, xparam_selected = a$param$selected, xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected
      ),
      templ_ui_constraint(ns, label = "Data Constraint"),
      ui_arbitrary_lines(id = ns("hline_arb"), a$hline_arb, a$hline_arb_label, a$hline_arb_color),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("xrange_scale"),
            label = "X-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          toggle_slider_ui(
            ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("comb_line"), "Display Combined line", a$comb_line),
          checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab)
        ),
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            ns("line_size"),
            "Line Size",
            value_min_max = a$line_size,
            step = .25,
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

srv_g_density_distribution_plot <- function(id, # nolint
                                            datasets,
                                            reporter,
                                            dataname,
                                            param_var,
                                            param,
                                            trt_group,
                                            color_manual,
                                            color_comb,
                                            plot_height,
                                            plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    anl_chunks <- constr_anl_chunks(
      session, input, datasets, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 2
    )

    # update sliders for axes taking constraints into account
    xrange_slider <- toggle_slider_server("xrange_scale")
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(session, input, xrange_slider$update_state, "xaxis_var", "xaxis_param", anl_chunks)
    keep_range_slider_updated(
      session,
      input,
      yrange_slider$update_state,
      "xaxis_var",
      "xaxis_param",
      anl_chunks,
      is_density = TRUE
    )
    keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    create_plot <- reactive({
      validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
      private_chunks <- teal.code::chunks_deep_clone(anl_chunks()$chunks)

      # nolint start
      param <- input$xaxis_param
      xaxis_var <- input$xaxis_var
      xlim <- xrange_slider$state()$value
      ylim <- yrange_slider$state()$value
      font_size <- input$font_size
      line_size <- input$line_size
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      facet_ncol <- input$facet_ncol
      validate(need(
        is.na(facet_ncol) || (as.numeric(facet_ncol) > 0 && as.numeric(facet_ncol) %% 1 == 0),
        "Number of plots per row must be a positive integer"
      ))
      comb_line <- input$comb_line
      rug_plot <- input$rug_plot
      rotate_xlab <- input$rotate_xlab
      trt_group <- input$trt_group
      # nolint end
      validate(need(input$trt_group, "Please select a treatment variable"))

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "density_distribution",
        expression = bquote({
          p <- goshawk::g_density_distribution_plot(
            data = ANL,
            param_var = .(param_var),
            param = .(param),
            xaxis_var = .(xaxis_var),
            trt_group = .(trt_group),
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
        })
      )

      teal.code::chunks_safe_eval(private_chunks)

      private_chunks
    })

    create_table <- reactive({
      private_chunks <- teal.code::chunks_deep_clone(create_plot())

      param <- input$xaxis_param
      xaxis_var <- input$xaxis_var
      font_size <- input$font_size
      trt_group <- input$trt_group

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "table",
        expression = bquote({
          tbl <- goshawk::t_summarytable(
            data = ANL,
            trt_group = .(trt_group),
            param_var = .(param_var),
            param = .(param),
            xaxis_var = .(xaxis_var),
            font_size = .(font_size)
          )
        })
      )

      teal.code::chunks_safe_eval(private_chunks)
      private_chunks
    })

    main_code <- reactive({
      private_chunks <- create_table()
      teal.code::chunks_push(
        chunks = private_chunks,
        id = "output",
        expression = quote(print(p))
      )

      teal.code::chunks_safe_eval(private_chunks)

      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(private_chunks)

      private_chunks
    })

    plot_r <- reactive({
      teal.code::chunks_get_var("p", main_code())
    })

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Density Distribution Plot")
        card$append_text("Density Distribution Plot", "header2")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Selected Options", "header3")
        card$append_text(
          formatted_data_constraint(input$constraint_var, input$constraint_range_min, input$constraint_range_max)
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
        card$append_text("Descriptive Statistics", "header3")
        card$append_table(
          teal.code::chunks_get_var("tbl", main_code()) %>%
            dplyr::mutate_if(is.numeric, round, 2)
        )
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_text("Show R Code", "header3")
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 1L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }

      teal.reporter::add_card_button_srv("addReportCard", reporter = reporter, card_fun = card_fun)
      teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
      teal.reporter::reset_report_button_srv("resetButton", reporter)
    }
    ###

    output$table_ui <- DT::renderDataTable({
      tbl <- teal.code::chunks_get_var("tbl", main_code())
      numeric_cols <- names(dplyr::select_if(tbl, is.numeric))

      DT::datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 2)
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = "Density Distribution Plot"
    )
  })
}
