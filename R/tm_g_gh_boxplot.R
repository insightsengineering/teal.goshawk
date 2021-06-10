#' Box Plot
#'
#' This teal module renders the UI and calls the functions that create a box plot and accompanying
#' summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured
#'  laboratory data frame ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param list of biomarkers of interest.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL.
#' @param xaxis_var variable to categorize the x-axis.
#' @param facet_var variable to facet the plots by.
#' @param trt_group  \code{\link[teal]{choices_selected}} object with available choices and pre-selected option
#'  for variable names representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param loq_legend loq legend toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position a horizontal line.  NULL = No line.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param alpha numeric vector to define transparency of plotted points.
#'
#' @inheritParams teal.devel::standard_layout
#'
#' @import DescTools
#' @import utils
#' @import dplyr
#' @import goshawk
#' @import teal
#' @import teal.devel
#'
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD",
#'                     "B: Placebo" = "Placebo",
#'                     "C: Combination" = "Combination")
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(ADSL, cached = TRUE)
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
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
#'             AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD),
#'     ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'     ACTARM = factor(ACTARM) %>% reorder(TRTORD))
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ACTARM"]], 'label') <- var_labels[["ACTARM"]]
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     adsl <- cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "ADLB <- radlb(ADSL, cached = TRUE)
#'            var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'            ADLB <- ADLB %>%
#'              mutate(AVISITCD = case_when(
#'                  AVISIT == 'SCREENING' ~ 'SCR',
#'                  AVISIT == 'BASELINE' ~ 'BL',
#'                  grepl('WEEK', AVISIT) ~
#'                    paste('W', stringr::str_extract(AVISIT, '(?<=(WEEK ))[0-9]+')),
#'                  TRUE ~ as.character(NA)),
#'                AVISITCDN = case_when(
#'                  AVISITCD == 'SCR' ~ -2,
#'                  AVISITCD == 'BL' ~ 0,
#'                  grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'                  TRUE ~ as.numeric(NA)),
#'                AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'                TRTORD = case_when(
#'                  ARMCD == 'ARM C' ~ 1,
#'                  ARMCD == 'ARM B' ~ 2,
#'                  ARMCD == 'ARM A' ~ 3),
#'              ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'              ARM = factor(ARM) %>% reorder(TRTORD),
#'              ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'              ACTARM = factor(ACTARM) %>% reorder(TRTORD))
#'           attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'           attr(ADLB[['ACTARM']], 'label') <- var_labels[['ACTARM']]",
#'       vars = list(ADSL = adsl, arm_mapping = arm_mapping)),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'       tm_g_gh_boxplot(
#'         label = "Box Plot",
#'         dataname = "ADLB",
#'         param_var = "PARAMCD",
#'         param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'         yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
#'         xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
#'         facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
#'         trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'         loq_legend = TRUE,
#'         rotate_xlab = FALSE
#'       )
#'   )
#' )
#'
#'\dontrun{
#' shinyApp(app$ui, app$server)
#'
#'}
#'
tm_g_gh_boxplot <- function(label,
                            dataname,
                            param_var,
                            param,
                            yaxis_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
                            xaxis_var = choices_selected("AVISITCD", "AVISITCD"),
                            facet_var = choices_selected(c("ARM", "ACTARM"), "ARM"),
                            trt_group,
                            color_manual = NULL,
                            shape_manual = NULL,
                            facet_ncol = NULL,
                            loq_legend = TRUE,
                            rotate_xlab = FALSE,
                            hline = NULL,
                            plot_height = c(600, 200, 2000),
                            plot_width = NULL,
                            font_size = c(12, 8, 20),
                            dot_size = c(2, 1, 12),
                            alpha = c(0.8, 0.0, 1.0),
                            pre_output = NULL,
                            post_output = NULL) {
  stopifnot(
    is_character_single(label),
    is_character_single(dataname),
    is_character_single(param_var),
    is.choices_selected(param),
    is.choices_selected(yaxis_var),
    is.choices_selected(xaxis_var),
    is.choices_selected(facet_var),
    is.null(facet_ncol) || is_integer_single(facet_ncol),
    is_logical_single(loq_legend),
    is_logical_single(rotate_xlab),
    is.null(hline) || is_numeric_single(hline),
    is_numeric_vector(font_size) && length(font_size) == 3,
    is_numeric_vector(dot_size) && length(dot_size) == 3,
    is_numeric_vector(alpha) && length(alpha) == 3,
    is.choices_selected(trt_group)
  )
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual,
                       plot_height = plot_height,
                       plot_width = plot_width
    ),
    ui = ui_g_boxplot,
    ui_args = args
  )
}

ui_g_boxplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = div(
      fluidRow(
        plot_with_settings_ui(id = ns("boxplot"))
      ),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Selected Data Points"),
        DT::dataTableOutput(ns("brush_data"))
      )),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Descriptive Statistics"),
        DT::dataTableOutput(ns("table_ui"))
      ))
    ),
    encoding =  div(
      templ_ui_dataname(a$dataname),
      optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = a$trt_group$choices,
        selected = a$trt_group$selected,
        multiple = FALSE),
      templ_ui_params_vars(
        ns,
        xparam_choices = a$param$choices,
        xparam_selected = a$param$selected,
        xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices,
        xselected = a$xaxis_var$selected,
        ychoices = a$yaxis_var$choices,
        yselected = a$yaxis_var$selected
      ),
      optionalSelectInput(
        ns("facet_var"),
        label = "Facet by",
        choices = a$facet_var$choices,
        selected = a$facet_var$selected,
        multiple = FALSE),
      templ_ui_constraint(ns, label = "Data Constraint"), # required by constr_anl_chunks
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", a$loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("font_size"),  "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("alpha"), "Dot Alpha", a$alpha, ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_boxplot <- function(input,
                          output,
                          session,
                          datasets,
                          dataname,
                          param_var,
                          trt_group,
                          facet_var,
                          color_manual,
                          shape_manual,
                          plot_height,
                          plot_width) {
  init_chunks()

  # reused in all modules
  anl_chunks <- constr_anl_chunks(
    session, input, datasets, dataname,
    param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 2
  )
  # update sliders for axes taking constraints into account
  yrange_slider <- callModule(toggle_slider_server, "yrange_scale")
  keep_range_slider_updated(
    session,
    input,
    update_slider_fcn = yrange_slider$update_state,
    id_var = "yaxis_var",
    id_param_var = "xaxis_param",
    reactive_ANL = anl_chunks
  )
  keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

  create_plot <- reactive({
    private_chunks <- anl_chunks()$chunks$clone(deep = TRUE)
    # nolint start
    param <- input$xaxis_param
    yaxis <- input$yaxis_var
    xaxis <- input$xaxis_var
    facet_var <- if (is.null(input$facet_var)) "None" else input$facet_var
    yrange_scale <- yrange_slider$state()$value
    facet_ncol <- input$facet_ncol
    alpha <- input$alpha
    font_size <- input$font_size
    dot_size <- input$dot_size
    loq_legend <- input$loq_legend
    rotate_xlab <- input$rotate_xlab
    hline <- input$hline
    trt_group <- input$trt_group
    # nolint end
    validate(need(input$trt_group, "Please select a treatment variable"))
    validate(need(!is.null(xaxis), "Please select an X-Axis Variable"))
    validate(need(!is.null(yaxis), "Please select a Y-Axis Variable"))
    validate_has_variable(
      anl_chunks()$ANL,
      yaxis,
      sprintf("Variable %s is not available in data %s", yaxis, dataname))
    validate_has_variable(
      anl_chunks()$ANL,
      xaxis,
      sprintf("Variable %s is not available in data %s", xaxis, dataname))

    if (!facet_var == "None") {
      validate_has_variable(
        anl_chunks()$ANL,
        facet_var,
        sprintf("Variable %s is not available in data %s", facet_var, dataname))
    }

    validate(need(
      !facet_var %in% c("ACTARM", "ARM")[!c("ACTARM", "ARM") %in% trt_group],
      sprintf("You can not choose %s as facetting variable for treatment variable %s.", facet_var, trt_group)
      ))
    validate(need(
      !xaxis %in% c("ACTARM", "ARM")[!c("ACTARM", "ARM") %in% trt_group],
      sprintf("You can not choose %s as x-axis variable for treatment variable %s.", xaxis, trt_group)
    ))

    chunks_push(
      chunks = private_chunks,
      id = "boxplot",
      expression = bquote({
        p <- g_boxplot(
          data = ANL,
          biomarker = .(param),
          xaxis_var = .(xaxis),
          yaxis_var = .(yaxis),
          hline = .(`if`(is.na(hline), NULL, as.numeric(hline))),
          facet_ncol = .(facet_ncol),
          loq_legend = .(loq_legend),
          rotate_xlab = .(rotate_xlab),
          trt_group = .(trt_group),
          ymin_scale = .(yrange_scale[[1]]),
          ymax_scale = .(yrange_scale[[2]]),
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

    chunks_safe_eval(private_chunks)

    private_chunks
  })

  create_table <- reactive({
    private_chunks <- create_plot()$clone(deep = TRUE)

    param <- input$xaxis_param
    xaxis_var <- input$yaxis_var #nolint
    font_size <- input$font_size
    trt_group <- input$trt_group

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
          visit_var = .("AVISITCD")
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
      expression = quote(print(p))
    )

    chunks_safe_eval(private_chunks)

    chunks_reset()
    chunks_push_chunks(private_chunks)

    private_chunks
  })

  plot_r <- reactive({
    chunks_get_var("p", main_code())
  })

  boxplot_data <- callModule(
    plot_with_settings_srv,
    id = "boxplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  output$table_ui <- DT::renderDataTable({
    tbl <- chunks_get_var("tbl", main_code())

    numeric_cols <- setdiff(names(select_if(tbl, is.numeric)), "n")

    DT::datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(numeric_cols, 4)

  })

  # highlight plot area
  output$brush_data <- DT::renderDataTable({
    boxplot_brush <- boxplot_data$brush()

    ANL <- isolate(anl_chunks()$ANL) %>% droplevels() #nolint
    validate_has_data(ANL, 2)

    xvar <- isolate(input$xaxis_var)
    yvar <- isolate(input$yaxis_var)
    facetv <- isolate(input$facet_var)
    trt_group <- isolate(input$trt_group)

    req(all(c(xvar, yvar, facetv, trt_group) %in% names(ANL)))

    df <- clean_brushedPoints(
      select(ANL, "USUBJID", trt_group, facetv, "AVISITCD", "PARAMCD", xvar, yvar, "LOQFL"),
      boxplot_brush
    )

    numeric_cols <- names(select_if(df, is.numeric))

    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(numeric_cols, 4)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Box Plot"
  )
}
