#' Teal Module: density distribution plot
#'
#' This module displays a density distribution plot.
#'
#' @param label menu item label of the module in the teal app 
#' @param dataname ADaM structured analysis laboratory (ADLB/ALB) data frame.  
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param xaxis_var_choices list of variables containing biomarker results choices.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LBLOQFL.
#' @param plot_width controls plot width.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param hline y-axis value to position a horizontal line.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param code_data_processing TODO
#'
#' @inheritParams teal::standard_layout
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' @import goshawk
#' @import shiny
#' @import teal
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details This module displays a density distribution plot. link to specification file \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#' 
#' param_choices <- c("ACIGG", "CRP", "ADIGG", "CCL20")
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'     tm_g_density_distribution_plot(
#'        label = "Density Distribution Plot",
#'        dataname = "ALB",
#'        param_var = "PARAMCD",
#'        param_choices = param_choices,
#'        param = param_choices[1],
#'        xaxis_var = "AVAL",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
#'        trt_group = "ARM",
#'        color_manual = color_manual,
#'        loq_flag_var = 'LOQFL',
#'        plot_width = c(800, 200, 2000),
#'        plot_height = c(500, 200, 2000),
#'        font_size = c(12, 8, 20),
#'        line_size = c(1, .25, 3)
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_density_distribution_plot <- function(label,
                                           dataname,
                                           param_var,
                                           param_choices = param,
                                           param,
                                           xaxis_var,
                                           xaxis_var_choices = xaxis_var,
                                           trt_group = "ARM",
                                           color_manual = NULL,
                                           loq_flag_var = 'LOQFL',
                                           plot_width = c(800, 200, 2000),
                                           plot_height = c(500, 200, 2000),
                                           font_size = c(12, 8, 20),
                                           line_size = c(1, .25, 3),
                                           hline = NULL,
                                           rotate_xlab = FALSE,
                                           pre_output = NULL,
                                           post_output = NULL,
                                           code_data_processing = NULL) {

  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_density_distribution_plot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       param = param,
                       xaxis_var = xaxis_var,
                       trt_group = trt_group,
                       color_manual = color_manual,
                       code_data_processing = code_data_processing
                       ),
    ui = ui_g_density_distribution_plot,
    ui_args = args
  )
  
}

ui_g_density_distribution_plot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = div(tagList(uiOutput(ns("plot_ui")), uiOutput(ns("table_ui")))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline, min = 0, max = 1, step = .1),
      optionalSliderInputValMinMax(ns("plot_width"), "Plot Width", a$plot_width, ticks = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      uiOutput(ns("xaxis_scale")),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("line_size"), "Line Size", a$line_size, value_min_max = c(1, .25, 3), step = .25, ticks = FALSE)
    ),
    # forms = tags$div(
    #   actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
    #   # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    # ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_density_distribution_plot <- function(input, output, session, datasets, dataname, param_var, param, 
                                            xaxis_var, trt_group, color_manual, code_data_processing) {

  ns <- session$ns # must add for the dynamic ui.range_scale field
  
  # dynamic plot width
  output$plot_ui <- renderUI({
    plot_width <- input$plot_width
    validate(need(plot_width, "need valid plot width"))
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("density_distribution_plot"), width = plot_width, height = plot_height)
    })
  
  # filter data by param and the xmin and xmax values
  filter_ALB <- reactive({

    param <- input$param
    
    xmin_scale <- -Inf
    xmax_scale <- Inf
    
    if (length(input$xrange_scale)){
        xmin_scale <- input$xrange_scale[1]
        xmax_scale <- input$xrange_scale[2]
    }
    
    datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter(eval(parse(text = param_var)) == param &
               xmin_scale <= eval(parse(text = xaxis_var)) &
               eval(parse(text = xaxis_var)) <= xmax_scale |
               is.na(xaxis_var))
  })
  
  # dynamic slider for x-axis
  output$xaxis_scale <- renderUI({
    ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # must add for the dynamic ui.range_scale field
    param <- input$param # must add for the dynamic ui.range_scale field
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == param)
      # identify min and max values of BM range ignoring NA values
      xmin_scale <- RoundTo(min(scale_data[[input$xaxis_var]], na.rm = TRUE), multiple = .001, FUN = floor)
      xmax_scale <- RoundTo(max(scale_data[[input$xaxis_var]], na.rm = TRUE), multiple = .001, FUN = ceiling)
      
      tagList({
        sliderInput(ns("xrange_scale"), label="X-Axis Range Scale", xmin_scale, xmax_scale, value = c(xmin_scale, xmax_scale))
      })
  })

  output$density_distribution_plot <- renderPlot({
    ALB <- filter_ALB()
    param <- input$param
    xaxis_var <- input$xaxis_var
    font_size <- input$font_size
    line_size <- input$line_size
    hline <- as.numeric(input$hline)
    rotate_xlab <- input$rotate_xlab

    validate(need(!is.null(ALB) && is.data.frame(ALB), "No Data Left"))
    validate(need(nrow(ALB) > 0 , "No Observations Left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(xaxis_var, "No Valid X-Axis Variable Selected"))
    validate(need(trt_group %in% names(ALB),
                  paste("variable", trt_group, " is not available in data", dataname)))
    validate(need(xaxis_var %in% names(ALB),
                  paste("variable", xaxis_var, " is not available in data", dataname)))

      p <- goshawk:::g_density_distribution_plot(
        data = ALB,
        param_var = param_var,
        param = param,
        xaxis_var = xaxis_var,
        trt_group = trt_group,
        color_manual = color_manual,
        font_size = font_size,
        line_size = line_size,
        rotate_xlab = rotate_xlab,
        hline = hline
    )

     p

  })

 output$table_ui <- renderTable({

   ALB <- filter_ALB()
   param <- input$param
   xaxis_var <- input$xaxis_var
   font_size <- input$font_size

   t <- goshawk:::t_summarytable(
     data = ALB,
     trt_group = trt_group,
     param_var = param_var,
     param = param,
     xaxis_var = xaxis_var,
     font_size = font_size
  )

   t

  })

}
