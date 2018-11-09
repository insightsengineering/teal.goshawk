#' Teal Module: density distribution plot
#'
#' This module displays the web user interface and calls the functions that create 
#' a density distribution plot and an accompanying summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname ADaM structured analysis laboratory data frame e.g. ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param xaxis_var_choices list of variables containing biomarker results choices.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LBLOQFL.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param hline y-axis value to position a horizontal line.
#' @param facet_ncol numeric value indicating number of facets per row.
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
#' @details None
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#' 
#' param_choices <- c("ACIGG", "CRP", "ADIGG", "CCL20", "IGG")
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
                                           plot_height = c(500, 200, 2000),
                                           font_size = c(12, 8, 20),
                                           line_size = c(1, .25, 3),
                                           hline = NULL,
                                           facet_ncol = 2,
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
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),
      radioButtons(ns("constraint_var"), "Data Constraint", c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")),
      uiOutput(ns("constraint_min_value"), style="display: inline-block; vertical-align:center"),
      uiOutput(ns("constraint_max_value"), style="display: inline-block; vertical-align:center"),

      tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;"),
      uiOutput(ns("xaxis_zoom")),
      numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
      checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a Horizontal Line:", a$hline, min = 0, max = 1, step = .1),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
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
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("density_distribution_plot"), height = plot_height)
    })
  
  # dynamic slider for x-axis
  output$xaxis_zoom <- renderUI({
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param 
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == param)
    
    # establish default value during reaction and prior to value being available
    xmin_scale <- -Inf
    xmax_scale <- Inf
    
    # identify min and max values of BM range ignoring NA values
    xmin_scale <- min(scale_data[[input$xaxis_var]], na.rm = TRUE)
    xmax_scale <- max(scale_data[[input$xaxis_var]], na.rm = TRUE)
    
    ran <- xmax_scale - xmin_scale
    
    tagList({
      sliderInput(ns("xrange_scale"), label="X-Axis Range Zoom", 
                  floor(xmin_scale), ceiling(xmax_scale), 
                  value = c(floor(xmin_scale), ceiling(xmax_scale)))
    })
    
  })

  # filter data by param and the constraint_min and constraint_max values
  filter_ALB <- reactive({
    param <- input$param
    constraint_var <- input$constraint_var

    if (constraint_var != "NONE"){
    constraint_min_range <- -Inf
    constraint_max_range <- Inf
    
    if (length(input$constraint_min)){
      constraint_min_range <- input$constraint_min
    }
    
    if (length(input$constraint_max)){
      constraint_max_range <- input$constraint_max
    }
    datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter(eval(parse(text = param_var)) == param &
               constraint_min_range <= eval(parse(text = constraint_var)) &
               eval(parse(text = constraint_var)) <= constraint_max_range |
               is.na(constraint_var)
               )
    } else{
      datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
        filter(eval(parse(text = param_var)) == param)
    }
  })

  # minimum data constraint value
  output$constraint_min_value <- renderUI({
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
      param <- input$param
      scale_data <- ALB %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint vatriable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & visitFreq[1] == "SCR" | 
          input$constraint_var == "BASE" & (visitFreq[1] == "BL" | visitFreq[2] == "BL")){
        # identify min and max values of constraint var range ignoring NA values
        constraint_min_range <- min(scale_data[[input$constraint_var]], na.rm = TRUE)
        constraint_max_range <- max(scale_data[[input$constraint_var]], na.rm = TRUE)
        
        tagList({
          numericInput(ns("constraint_min"), 
                       paste0("Min (", constraint_min_range, ")"), 
                       value = constraint_min_range, 
                       min = constraint_min_range, max = constraint_max_range)
        })
      }
    }
    else {
      return(NULL)
    }

  })

  # maximum data constraint value
  output$constraint_max_value <- renderUI({
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
      param <- input$param
      scale_data <- ALB %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint vatriable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & visitFreq[1] == "SCR" | 
          input$constraint_var == "BASE" & (visitFreq[1] == "BL" | visitFreq[2] == "BL")){
        # identify min and max values of constraint var range ignoring NA values
        constraint_min_range <- min(scale_data[[input$constraint_var]], na.rm = TRUE)
        constraint_max_range <- max(scale_data[[input$constraint_var]], na.rm = TRUE)

          tagList({
            numericInput(ns("constraint_max"), 
                         paste0("Max (", constraint_max_range, ")"),
                         value = constraint_max_range, 
                         min = constraint_min_range, max = constraint_max_range)
          })
      }
    }
    else {
      return(NULL)
    }
  })

    output$density_distribution_plot <- renderPlot({
    ALB <- filter_ALB()
    param <- input$param
    xaxis_var <- input$xaxis_var
    xmin_scale <- input$xrange_scale[1]
    xmax_scale <- input$xrange_scale[2]
    font_size <- input$font_size
    line_size <- input$line_size
    hline <- as.numeric(input$hline)
    facet_ncol <- input$facet_ncol
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
        xmin = xmin_scale,
        xmax = xmax_scale,
        color_manual = color_manual,
        font_size = font_size,
        line_size = line_size,
        facet_ncol = facet_ncol,
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
