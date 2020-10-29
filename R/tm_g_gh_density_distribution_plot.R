#' Density Distribution Plot
#'
#' This teal module renders the UI and calls the functions that create a density distribution plot
#' and an accompanying summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param color_comb name or hex value for combined treatment color.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param hline y-axis value to position a horizontal line.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param comb_line display combined treatment line toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#'
#' @inheritParams teal.devel::standard_layout
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
#' \dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' ADLB <- ADLB %>%
#'   mutate(
#'     AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)
#'     ),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)
#'     ),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3
#'     ),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD)
#'   )
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code =
#'       '# original ARM value = dose value # nolint
#' arm_mapping <- list("A: Drug X" = "150mg QD",
#'                     "B: Placebo" = "Placebo",
#'                     "C: Combination" = "Combination")
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'     TRUE ~ as.character(NA)),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
#'           ',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_gh_density_distribution_plot(
#'       label = "Density Distribution Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = "ARM",
#'       color_manual = c("150mg QD" = "#000000",
#'                        "Placebo" = "#3498DB",
#'                        "Combination" = "#E74C3C"),
#'       color_comb = "#39ff14",
#'       comb_line = TRUE,
#'       plot_height = c(500, 200, 2000),
#'       font_size = c(12, 8, 20),
#'       line_size = c(1, .25, 3)
#'     )
#'   )
#' )
#'
#' shinyApp(app$ui, app$server)
#'
#' }
tm_g_gh_density_distribution_plot <- function(label, # nolint
                                              dataname,
                                              param_var,
                                              param,
                                              xaxis_var,
                                              trt_group = "ARM",
                                              color_manual = NULL,
                                              color_comb = NULL,
                                              plot_height = c(500, 200, 2000),
                                              plot_width = NULL,
                                              font_size = c(12, 8, 20),
                                              line_size = c(1, .25, 3),
                                              hline = NULL,
                                              facet_ncol = 2L,
                                              comb_line = TRUE,
                                              rotate_xlab = FALSE,
                                              pre_output = NULL,
                                              post_output = NULL) {
  stopifnot(
    is_character_single(label),

    is_character_single(dataname),
    is_character_single(param_var),
    is.choices_selected(param),
    is.choices_selected(xaxis_var),

    is_character_single(trt_group),
    # color_manual, color_comb
    is_numeric_vector(font_size) && length(font_size) == 3,
    is_numeric_vector(line_size) && length(line_size) == 3,
    is.null(hline) || is_numeric_single(hline),
    is_integer_single(facet_ncol),
    is_logical_single(comb_line),
    is_logical_single(rotate_xlab)
  )
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

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

  standard_layout(
    output = div(
      fluidRow(
        plot_with_settings_ui(id = ns("plot"), height = a$plot_height, width = a$plot_width)
      ),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Descriptive Statistics"),
        DT::dataTableOutput(ns("table_ui"))
      ))
    ),
    encoding = div(
      templ_ui_dataname(a$dataname),
      templ_ui_params_vars(
        ns,
        xparam_choices = a$param$choices, xparam_selected = a$param$selected, xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected
      ),
      templ_ui_constraint(ns, label = "Data Constraint"),
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("xrange_scale"),
            label = "X-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("comb_line"), "Display combination line", a$comb_line),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(
            ns("line_size"),
            "Line Size",
            value_min_max = a$line_size,
            step = .25,
            ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_density_distribution_plot <- function(input, # nolint
                                            output,
                                            session,
                                            datasets,
                                            dataname,
                                            param_var,
                                            param,
                                            trt_group,
                                            color_manual,
                                            color_comb,
                                            plot_height,
                                            plot_width) {
  anl_chunks <- constr_anl_chunks(
    session, input, datasets, dataname,
    param_id = "xaxis_param", param_var = param_var, trt_group = trt_group
  )

  # update sliders for axes taking constraints into account
  xrange_slider <- callModule(toggle_slider_server, "xrange_scale")
  keep_range_slider_updated(session, input, xrange_slider$update_state, "xaxis_var", "xaxis_param", anl_chunks)
  keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

  create_plot <- reactive({
    validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
    private_chunks <- anl_chunks()$chunks$clone(deep = TRUE)

    #nolint start
    param <- input$xaxis_param
    xaxis_var <- input$xaxis_var
    xmin_scale <- xrange_slider$state()$value[[1]]
    xmax_scale <- xrange_slider$state()$value[[2]]
    font_size <- input$font_size
    line_size <- input$line_size
    hline <- as.numeric(input$hline)
    hline <- `if`(is.na(hline), NULL, as.numeric(hline))
    facet_ncol <- as.integer(input$facet_ncol)
    comb_line <- input$comb_line
    rotate_xlab <- input$rotate_xlab
    #nolint end

    chunks_push(
      chunks = private_chunks,
      id = "density_distribution",
      expression = bquote({
        p <- g_density_distribution_plot(
          data = ANL,
          param_var = .(param_var),
          param = .(param),
          xaxis_var = .(xaxis_var),
          trt_group = .(trt_group),
          xmin = .(xmin_scale),
          xmax = .(xmax_scale),
          color_manual = .(color_manual),
          color_comb = .(color_comb),
          font_size = .(font_size),
          line_size = .(line_size),
          facet_ncol = .(facet_ncol),
          comb_line = .(comb_line),
          rotate_xlab = .(rotate_xlab),
          hline = .(hline)
        )
      })
    )

    chunks_safe_eval(private_chunks)

    private_chunks
  })

  create_table <- reactive({
    private_chunks <- create_plot()$clone(deep = TRUE)

    param <- input$xaxis_param
    xaxis_var <- input$xaxis_var
    font_size <- input$font_size

    chunks_push(
      chunks = private_chunks,
      id = "table",
      expression = bquote({
        tbl <- t_summarytable(
          data = ANL,
          trt_group = .(trt_group),
          param_var = .(param_var),
          param = .(param),
          xaxis_var = .(xaxis_var),
          font_size = .(font_size)
        )
      })
    )

    chunks_safe_eval(private_chunks)
    private_chunks
  })

  main_code <- reactive({
    private_chunks <- create_table()
    chunks_push(
      chunks = private_chunks,
      id = "output",
      expression = quote(print(plot))
    )
    init_chunks(private_chunks)
    private_chunks
  })

  density_distribution_plot_r <- reactive({
    chunks_get_var("p", main_code())
  })

  callModule(
    plot_with_settings_srv,
    id = "plot",
    plot_r = density_distribution_plot_r,
    height = plot_height,
    width = plot_width,
  )

  output$table_ui <- DT::renderDataTable({
    tbl <- chunks_get_var("tbl", main_code())

    numeric_cols <- names(select_if(tbl, is.numeric))

    DT::datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(numeric_cols, 2)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Scatter Plot"
  )
}
