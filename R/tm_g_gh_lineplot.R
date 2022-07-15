#' Line plot
#'
#' This teal module renders the UI and calls the function that creates a line plot.
#'
#' @inheritParams teal.widgets::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data that includes parameter labels.
#' @param xaxis_var single name of variable in analysis data that is used as x-axis in the plot for the
#' respective goshawk function.
#' @param xvar_level vector that can be used to define the factor level of xvar. Only use it when
#' xvar is character or factor.
#' @param filter_var data constraint variable.
#' @param filter_var_choices data constraint variable choices.
#' @param yaxis_var single name of variable in analysis data that is used as summary variable in the
#' respective gshawk function.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param shape_choices Vector or \code{choices_selected} object with names of ADSL variables which
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
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values.
#' Default value is waive().
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param plot_font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge controls the position dodge of error bar
#' @param count_threshold minimum count of observations (as listed in the output table) to plot
#' nodes on the graph
#' @param table_font_size controls the font size of values in the table
#' @param plot_relative_height_value numeric value between 500 and 5000 for controlling the starting value
#' of the relative plot height slider
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
#'
#' library(dplyr)
#' library(scda)
#' library(stringr)
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
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     adsl <- cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADLB", ADLB,
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
#'                     ARMCD == 'ARM A' ~ 3),
#'                   ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'                   ARM = factor(ARM) %>% reorder(TRTORD),
#'                   ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'                   ACTARM = factor(ACTARM) %>% reorder(TRTORD))
#'                attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'                attr(ADLB[['ACTARM']], 'label') <- var_labels[['ACTARM']]",
#'       vars = list(ADSL = adsl, arm_mapping = arm_mapping)
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_gh_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       shape_choices = c("SEX", "RACE"),
#'       xaxis_var = choices_selected("AVISITCD", "AVISITCD"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       hline_arb = c(20.5, 19.5),
#'       hline_arb_color = c("red", "green"),
#'       hline_arb_label = c("A", "B")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_gh_lineplot <- function(label,
                             dataname,
                             param_var,
                             param,
                             param_var_label = "PARAM",
                             xaxis_var,
                             yaxis_var,
                             xvar_level = NULL,
                             filter_var = yaxis_var,
                             filter_var_choices = filter_var,
                             trt_group,
                             trt_group_level = NULL,
                             shape_choices = NULL,
                             stat = "mean",
                             hline_arb = numeric(0),
                             hline_arb_color = "red",
                             hline_arb_label = "Horizontal line",
                             color_manual = NULL,
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
                             plot_relative_height_value = 1000) {
  logger::log_info("Initializing tm_g_gh_lineplot")
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  checkmate::assert_numeric(table_font_size, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    table_font_size[1],
    lower = table_font_size[2], upper = table_font_size[3],
    null.ok = TRUE, .var.name = "table_font_size"
  )

  checkmate::assert_number(plot_relative_height_value, lower = 500, upper = 5000)

  checkmate::assert_number(count_threshold)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  args <- as.list(environment())

  module(
    label = label,
    server = srv_lineplot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      trt_group = trt_group,
      color_manual = color_manual,
      xvar_level = xvar_level,
      trt_group_level = trt_group_level,
      shape_choices = shape_choices,
      param_var_label = param_var_label,
      xtick = xtick,
      xlabel = xlabel,
      plot_height = plot_height,
      plot_width = plot_width
    ),
    ui = ui_lineplot,
    ui_args = args,
    filters = dataname
  )
}

ui_lineplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::plot_with_settings_ui(id = ns("plot")),
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
        # xparam and yparam are identical, so we only show the user one
        xparam_choices = a$param$choices, xparam_selected = a$param$selected, xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected,
        ychoices = a$yaxis_var$choices, yselected = a$yaxis_var$selected
      ),
      uiOutput(ns("shape_ui")),
      radioButtons(ns("stat"), "Select a Statistic:", c("mean", "median"), a$stat),
      checkboxInput(ns("include_stat"), "Include Statistic Table", value = TRUE),
      div(
        sliderInput(
          ns("relative_height"),
          div(
            "Relative height of plot to table(s)",
            title =
              paste(
                "The larger the value selected the greater the size of the plot relative\nto",
                "the size of the tables. Note the units of this slider are arbitrary.\nTo",
                "change the total size of the plot and table(s)\nuse",
                "the plot resizing controls available at the top right of the plot."
              ),
            icon("info-circle")
          ),
          min = 500,
          max = 5000,
          step = 50,
          value = a$plot_relative_height_value,
          ticks = FALSE
        ),
      ),
      templ_ui_constraint(ns), # required by constr_anl_chunks
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
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("count_threshold"), "Contributing Observations Threshold:", a$count_threshold)
        ),
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("dodge"), "Error Bar Position Dodge", a$dodge, ticks = FALSE),
          teal.widgets::panel_group(
            teal.widgets::panel_item(
              title = "Line Settings",
              uiOutput(ns("lines"))
            ),
            teal.widgets::panel_item(
              title = "Symbol settings",
              uiOutput(ns("symbols"))
            )
          ),
          teal.widgets::optionalSliderInputValMinMax(ns("plot_font_size"), "Font Size", a$plot_font_size, ticks = FALSE)
        ),
        teal.widgets::panel_item(
          title = "Table settings",
          teal.widgets::optionalSliderInputValMinMax(
            ns("table_font_size"),
            "Table Font Size",
            a$table_font_size,
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

srv_lineplot <- function(id,
                         datasets,
                         reporter,
                         dataname,
                         param_var,
                         trt_group,
                         color_manual,
                         xvar_level,
                         trt_group_level,
                         shape_choices,
                         param_var_label,
                         xtick,
                         xlabel,
                         plot_height,
                         plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    ns <- session$ns
    output$shape_ui <- renderUI({
      if (!is.null(shape_choices)) {
        if (methods::is(shape_choices, "choices_selected")) {
          choices <- shape_choices$choices
          selected <- shape_choices$selected
        } else {
          choices <- shape_choices
          selected <- NULL
        }
        teal.widgets::optionalSelectInput(
          ns("shape"),
          "Select Line Splitting Variable",
          choices = choices, selected = selected
        )
      }
    })

    anl_chunks <- constr_anl_chunks(
      session = session,
      input = input,
      datasets = datasets,
      dataname = dataname,
      param_id = "xaxis_param",
      param_var = param_var,
      trt_group = input$trt_group,
      min_rows = 2
    )
    keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

    yrange_slider <- toggle_slider_server("yrange_scale")

    # update sliders for axes
    observe({
      varname <- input[["yaxis_var"]]
      validate(need(varname, "Please select variable"))

      ANL <- anl_chunks()$ANL # nolint
      validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

      shape <- if (!(is.null(input$shape) || input$shape == "None")) {
        input$shape
      } else {
        NULL
      }

      # we don't need to additionally filter for paramvar here as in keep_range_slider_updated because
      # xaxis_var and yaxis_var are always distinct
      sum_data <- ANL %>%
        dplyr::group_by_at(c(input$xaxis_var, input$trt_group, shape)) %>%
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

      # we don't use keep_range_slider_updated because this module computes the min, max
      # not from the constrained ANL, but rather by first grouping and computing confidence
      # intervals
      isolate(yrange_slider$update_state(
        min = minmax[[1]],
        max = minmax[[2]],
        value = minmax
      ))
    })


    line_color_start <- if (is.null(color_manual)) {
      c("#ff0000", "#008000", "#4ca3dd", "#8a2be2")
    } else {
      color_manual
    }
    line_color_defaults <- reactiveVal(line_color_start)

    line_type_defaults <- reactiveVal("solid")

    observeEvent(input$trt_group, {
      req(input$trt_group)
      anl_arm <- anl_chunks()$ANL[[input$trt_group]]
      anl_arm_nlevels <- nlevels(anl_arm)

      if (is.null(names(line_color_defaults()))) {
        # if color_manual did not specify arms (i.e. didn't have names) then order
        # of the vector does not need to match order of level(anl_arm)
        line_color_to_set <- line_color_defaults()[seq_len(anl_arm_nlevels)]
      } else {
        # if color_manual did specify arms then we need to make sure the order of
        # line_color_to_set matches the order of level(anl_arm) and if any arms are invalid
        # or missing then we fill with a random colour
        line_color_to_set <- stats::setNames(line_color_defaults()[levels(anl_arm)], nm = levels(anl_arm))
      }
      line_color_to_set[is.na(line_color_to_set)] <- grDevices::rainbow(anl_arm_nlevels)[is.na(line_color_to_set)]
      line_color_defaults(line_color_to_set)

      line_type_to_set <- if (length(line_type_defaults()) <= anl_arm_nlevels) {
        c(line_type_defaults(), rep(line_type_defaults(), anl_arm_nlevels - length(line_type_defaults())))
      } else {
        line_type_defaults()[seq_len(anl_arm_nlevels)]
      }

      line_type_defaults(line_type_to_set)
    })

    line_color_selected <- reactive({
      if (is.null(input$trt_group)) {
        return(NULL)
      }
      anl_arm <- isolate(anl_chunks()$ANL[[input$trt_group]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)

      stats::setNames(
        vapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- input[[paste0("line_color_", idx)]]
            if (is.null(x)) isolate(line_color_defaults())[[idx]] else x
          },
          character(1)
        ),
        anl_arm_levels
      )
    })

    line_type_selected <- reactive({
      if (is.null(input$trt_group)) {
        return(NULL)
      }
      anl_arm <- isolate(anl_chunks()$ANL[[input$trt_group]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)

      stats::setNames(
        vapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- input[[paste0("line_type_", idx)]]
            if (is.null(x)) isolate(line_type_defaults())[[idx]] else x
          },
          character(1)
        ),
        anl_arm_levels
      )
    })

    output$lines <- renderUI({
      req(input$trt_group)
      anl_arm <- isolate(anl_chunks()$ANL[[input$trt_group]])
      anl_arm_nlevels <- nlevels(anl_arm)
      anl_arm_levels <- levels(anl_arm)
      color_def <- line_color_defaults()
      type_def <- line_type_defaults()
      tagList(
        lapply(
          seq_len(anl_arm_nlevels),
          function(idx) {
            x <- anl_arm_levels[[idx]]
            x_color <- color_def[[idx]]
            color_input <- colourpicker::colourInput(
              ns(paste0("line_color_", idx)),
              "Color:",
              x_color
            )
            x_type <- type_def[[idx]]
            type_input <- selectInput(
              ns(paste0("line_type_", idx)),
              "Type:",
              choices = c(
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
              ),
              selected = x_type
            )
            fluidRow(
              column(
                width = 12,
                tags$label("Line configuration for:", tags$code(x))
              ),
              column(
                width = 12,
                div(
                  style = "width: 50%; float: left;",
                  color_input
                ),
                div(
                  style = "width: 50%; float: left;",
                  type_input
                )
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
      eventExpr = input$shape,
      handlerExpr = symbol_type_defaults(symbol_type_start),
      ignoreNULL = TRUE
    )

    observe({
      req(input$shape)
      anl_shape <- anl_chunks()$ANL[[input$shape]]
      anl_shape_nlevels <- nlevels(anl_shape)
      symbol_type_to_set <- symbol_type_defaults()[pmin(length(symbol_type_defaults()), seq_len(anl_shape_nlevels))]
      symbol_type_defaults(symbol_type_to_set)
    })

    symbol_type_selected <- reactive({
      if (is.null(input$shape)) {
        return(NULL)
      }
      anl_shape <- isolate(anl_chunks()$ANL[[input$shape]])
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
      validate(need(input$shape, "Please select line splitting variable first."))

      anl_shape <- isolate(anl_chunks()$ANL[[input$shape]])
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

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    plot_r <- reactive({
      ac <- anl_chunks()
      private_chunks <- teal.code::chunks_deep_clone(ac$chunks)

      # nolint start
      ylim <- yrange_slider$state()$value
      plot_font_size <- input$plot_font_size
      dodge <- input$dodge
      rotate_xlab <- input$rotate_xlab
      count_threshold <- `if`(is.na(as.numeric(input$count_threshold)), 0, as.numeric(input$count_threshold))
      table_font_size <- input$table_font_size

      median <- ifelse(input$stat == "median", TRUE, FALSE)
      relative_height <- input$relative_height
      validate(need(input$trt_group, "Please select a treatment variable"))
      trt_group <- input$trt_group
      color_selected <- line_color_selected()
      type_selected <- line_type_selected()
      symbol_selected <- symbol_type_selected()
      include_stat <- input$include_stat
      # nolint end

      validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
      validate(need(input$yaxis_var, "Please select a Y-Axis Variable"))

      # nolint start
      param <- input$xaxis_param
      xaxis <- input$xaxis_var
      yaxis <- input$yaxis_var
      # nolint end

      shape <- if (!(is.null(input$shape) || input$shape == "None")) {
        input$shape
      } else {
        NULL
      }

      teal.code::chunks_validate_custom(
        bquote(nrow(ANL[complete.cases(ANL[, c(.(yaxis), .(xaxis))]), ]) >= 2),
        "Number of complete rows on x and y axis variables is less than 2",
        chunks = private_chunks
      )

      if (!methods::is(xtick, "waiver") && !is.null(xtick)) {
        teal.code::chunks_push(
          chunks = private_chunks,
          expression = bquote({
            keep_index <- which(.(xtick) %in% ANL[[.(xaxis)]])
            xtick <- (.(xtick))[keep_index] # extra parentheses needed for edge case, e.g. 1:5[keep_index]
            xlabel <- (.(xlabel))[keep_index]
          })
        )
      }

      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color

      teal.code::chunks_push(
        chunks = private_chunks,
        id = "lineplot",
        expression = bquote({
          p <- goshawk::g_lineplot(
            data = ANL[complete.cases(ANL[, c(.(yaxis), .(xaxis))]), ],
            biomarker_var = .(param_var),
            biomarker_var_label = .(param_var_label),
            biomarker = .(param),
            value_var = .(yaxis),
            ylim = .(ylim),
            trt_group = .(trt_group),
            trt_group_level = .(trt_group_level),
            shape = .(shape),
            shape_type = .(symbol_selected),
            time = .(xaxis),
            time_level = .(xvar_level),
            color_manual = .(color_selected),
            line_type = .(type_selected),
            median = .(median),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            xtick = .(if (!methods::is(xtick, "waiver") && !is.null(xtick)) quote(xtick) else xtick),
            xlabel = .(if (!methods::is(xtick, "waiver") && !is.null(xtick)) quote(xlabel) else xlabel),
            rotate_xlab = .(rotate_xlab),
            plot_height = .(relative_height), # in g_lineplot this is relative height of plot to table
            plot_font_size = .(plot_font_size),
            dodge = .(dodge),
            count_threshold = .(count_threshold),
            table_font_size = .(table_font_size),
            display_center_tbl = .(include_stat)
          )
          print(p)
        })
      )

      teal.code::chunks_safe_eval(private_chunks)

      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(private_chunks)

      teal.code::chunks_get_var("p")
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
        card$set_name("Line Plot")
        card$append_text("Line Plot", "header2")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Selected Options", "header3")
        card$append_text(
          paste(
            formatted_data_constraint(input$constraint_var, input$constraint_range_min, input$constraint_range_max),
            "\nSelect Line Splitting Variable:",
            if (!is.null(input$shape)) input$shape else "None",
            "\nContributing Observations Threshold:",
            input$count_threshold
          ),
          style = "verbatim"
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
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

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = "Line Plot"
    )
  })
}
