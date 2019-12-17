#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description TODO: a bit more info why the module is needed
#'
#' @inheritParams teal.devel::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured laboratory data frame
#'   \code{ADLB}.
#' @param param_var name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. \code{BASE}.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. \code{AVAL}.
#' @param trt_group name of variable representing treatment group e.g. \code{ARM}.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
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
#' library(random.cdisc.data)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD",
#'                     "B: Placebo" = "Placebo",
#'                     "C: Combination" = "Combination")
#'
#' ADSL <- radsl(N = 20, seed = 1)
#' ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)),
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
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = {'
#'       arm_mapping <- list("A: Drug X" = "150mg QD",
#'                           "B: Placebo" = "Placebo",
#'                           "C: Combination" = "Combination")
#'
#'       ADSL <- radsl(N = 20, seed = 1)
#'       ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#'       ADLB <- ADLB %>%
#'         mutate(AVISITCD = case_when(
#'             AVISIT == "SCREENING" ~ "SCR",
#'             AVISIT == "BASELINE" ~ "BL",
#'             grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'             TRUE ~ as.character(NA)),
#'           AVISITCDN = case_when(
#'             AVISITCD == "SCR" ~ -2,
#'             AVISITCD == "BL" ~ 0,
#'             grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'             TRUE ~ as.numeric(NA)),
#'             AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'           TRTORD = case_when(
#'             ARMCD == "ARM C" ~ 1,
#'             ARMCD == "ARM B" ~ 2,
#'             ARMCD == "ARM A" ~ 3),
#'           ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'           ARM = factor(ARM) %>% reorder(TRTORD))
#'           '},
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'        label = "Scatter Plot",
#'        dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'        xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
#'        yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'        trt_group = "ARM",
#'        color_manual = c("150mg QD" = "#000000",
#'                         "Placebo" = "#3498DB",
#'                         "Combination" = "#E74C3C"),
#'        shape_manual = c("N"  = 1, "Y"  = 2, "NA" = 0),
#'        plot_height = c(500, 200, 2000),
#'        facet_ncol = 2,
#'        facet = FALSE,
#'        facet_var = "ARM",
#'        reg_line = FALSE,
#'        font_size = c(12, 8, 20),
#'        dot_size = c(1, 1, 12),
#'        reg_text_size = c(3, 3, 10)
#'    )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                             dataname,
                             param_var,
                             param,
                             xaxis_var,
                             yaxis_var,
                             trt_group = "ARM",
                             color_manual = NULL,
                             shape_manual = NULL,
                             facet_ncol = 2,
                             facet = FALSE,
                             facet_var = "ARM",
                             reg_line = FALSE,
                             rotate_xlab = FALSE,
                             hline = NULL,
                             vline = NULL,
                             plot_height = c(500, 200, 2000),
                             font_size = c(12, 8, 20),
                             dot_size = c(1, 1, 12),
                             reg_text_size = c(3, 3, 10),
                             pre_output = NULL,
                             post_output = NULL) {

  stopifnot(is.choices_selected(param))
  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_scatterplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual
    ),
    ui = ui_g_scatterplot,
    ui_args = args
  )

}

#' @importFrom shinyjs hidden
ui_g_scatterplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding =  div(
      templ_ui_dataname(a$dataname),
      templ_ui_param(ns, a$param$choices, a$param$selected), # required by constr_anl_chunks
      templ_ui_xy_vars(ns, a$xaxis_var$choices, a$xaxis_var$selected,
                       a$yaxis_var$choices, a$yaxis_var$selected),
      templ_ui_constraint(ns), # required by constr_anl_chunks
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          sliderInput(ns("xrange_scale"), label = "X-Axis Range Zoom", min = 0, max = 1, value = c(0, 1)),
          sliderInput(ns("yrange_scale"), label = "Y-Axis Range Zoom", min = 0, max = 1, value = c(0, 1)),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("facet"), "Treatment Facetting", a$facet),
          checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline),
          numericInput(ns("vline"), "Add a vertical line:", a$vline)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
          optionalSliderInputValMinMax(ns("font_size"),  "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("reg_text_size"), "Regression Annotations Size", a$reg_text_size,
                                       ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

#' @importFrom goshawk g_scatterplot
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              dataname,
                              param_var,
                              trt_group,
                              facet_var,
                              color_manual,
                              shape_manual) {

  ns <- session$ns

  # reused in all modules
  anl_chunks <- constr_anl_chunks(session, input, datasets, dataname, "param", param_var, trt_group)

  # update sliders for axes
  keep_range_slider_updated(session, input, "xrange_scale", "xaxis_var", anl_chunks)
  keep_range_slider_updated(session, input, "yrange_scale", "yaxis_var", anl_chunks)

  # plot
  output$scatterplot <- renderPlot({

    ac <- anl_chunks()
    private_chunks <- ac$chunks$clone(deep = TRUE)

    xrange_scale <- input$xrange_scale
    yrange_scale <- input$yrange_scale
    facet_ncol <- input$facet_ncol
    facet <- input$facet
    reg_line <- input$reg_line
    font_size <- input$font_size
    dot_size <- input$dot_size
    reg_text_size <- input$reg_text_size
    rotate_xlab <- input$rotate_xlab
    hline <- input$hline
    vline <- input$vline

    # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_chunk()) and some inputs
    param <- isolate(input$param)
    xaxis <- isolate(input$xaxis_var)
    yaxis <- isolate(input$yaxis_var)

    chunks_push(
      chunks = private_chunks,
      id = "scatterplot",
      expression = bquote({
        # re-establish treatment variable label
        g_scatterplot(
          data = ANL,
          param_var = .(param_var),
          param = .(param),
          xaxis_var = .(xaxis),
          yaxis_var = .(yaxis),
          trt_group = .(trt_group),
          xmin = .(xrange_scale[1]),
          xmax = .(xrange_scale[2]),
          ymin = .(yrange_scale[1]),
          ymax = .(yrange_scale[2]),
          color_manual = .(color_manual),
          shape_manual = .(shape_manual),
          facet_ncol = .(facet_ncol),
          facet = .(facet),
          facet_var = .(facet_var),
          reg_line = .(reg_line),
          font_size = .(font_size),
          dot_size = .(dot_size),
          reg_text_size = .(reg_text_size),
          rotate_xlab = .(rotate_xlab),
          hline = .(`if`(is.na(hline), NULL, as.numeric(hline))),
          vline = .(`if`(is.na(vline), NULL, as.numeric(vline)))
        )
      })
    )

    p <- chunks_safe_eval(private_chunks)

    # promote chunks to be visible in the sessionData by other modules
    init_chunks(private_chunks)

    p
  })

  # dynamic plot height and brushing
  output$plot_ui <- renderUI({

    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))

    plotOutput(ns("scatterplot"),
               height = plot_height,
               brush = brushOpts(id = ns("scatterplot_brush"), resetOnNew = T)
    )
  })

  # highlight plot area
  output$brush_data <- DT::renderDataTable({
    req(input$scatterplot_brush)

    ANL <- isolate(anl_chunks()$ANL) # nolint
    validate_has_data(ANL, 5)

    xvar <- isolate(input$xaxis_var)
    yvar <- isolate(input$yaxis_var)

    req(all(c(xvar, yvar) %in% names(ANL)))

    df <- brushedPoints(
      select(ANL, "USUBJID", trt_group, "AVISITCD", "PARAMCD", xvar, yvar, "LOQFL"),
      input$scatterplot_brush
    )

    numeric_cols <- names(select_if(df, is.numeric))

    DT::datatable(df, rownames = FALSE) %>%
      DT::formatRound(numeric_cols, 4)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Scatter Plot"
  )
}
