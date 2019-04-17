#' Correlation Plot
#'
#' This teal module renders the UI and calls the function that creates a correlation plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured laboratory data frame ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param xaxis_param_choices list of biomarkers of interest.
#' @param xaxis_param biomarker selected.
#' @param yaxis_param_choices list of biomarkers of interest.
#' @param yaxis_param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. BASE.
#' @param xaxis_var_choices list of variables containing biomarker results choices.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL.
#' @param yaxis_var_choices list of variables containing biomarker results choices.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet TRUE.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#' @param code_data_processing TODO
#'
#' @inheritParams teal::standard_layout
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' @import goshawk
#' @import shiny
#' @import teal
#' @import tidyr
#'
#' @details This module displays a correlation plot.
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#'
#' param_choices <- c("CRP", "ADIGG", "CCL3", "CCL20")
#' xaxis_param_choices <- param_choices
#' yaxis_param_choices <- param_choices
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'     tm_g_correlationplot(
#'        label = "Correlation Plot",
#'        dataname = "ALB",
#'        param_var = "PARAMCD",
#'        xaxis_param_choices = param_choices,
#'        xaxis_param = param_choices[1],
#'        xaxis_var = "BASE",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "AVALL2"),
#'        yaxis_param_choices = param_choices,
#'        yaxis_param = param_choices[2],
#'        yaxis_var = "AVAL",
#'        yaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "AVALL2"),
#'        trt_group = "ACTARM",
#'        color_manual = color_manual,
#'        shape_manual = shape_manual,
#'        plot_height = c(500, 200, 2000),
#'        facet_ncol = 2,
#'        facet = FALSE,
#'        reg_line = FALSE,
#'        font_size = c(12, 8, 20),
#'        dot_size = c(1, 1, 12),
#'        reg_text_size = c(3, 3, 10)
#'    )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_correlationplot <- function(label,
                             dataname,
                             param_var,
                             xaxis_param = xaxis_param,
                             xaxis_param_choices = xaxis_param,
                             xaxis_var = xaxis_var,
                             xaxis_var_choices = xaxis_var,
                             yaxis_param = yaxis_param,
                             yaxis_param_choices = yaxis_param,
                             yaxis_var = yaxis_var, 
                             yaxis_var_choices = yaxis_var,
                             trt_group = "ARM",
                             color_manual = NULL,
                             shape_manual = NULL,
                             facet_ncol = 2,
                             facet = FALSE,
                             reg_line = FALSE,
                             rotate_xlab = FALSE,
                             hline = NULL,
                             vline = NULL,
                             plot_height = c(500, 200, 2000),
                             font_size = c(12, 8, 20),
                             dot_size = c(1, 1, 12),
                             reg_text_size = c(3, 3, 10),
                             pre_output = NULL,
                             post_output = NULL,
                             code_data_processing = NULL) {

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_correlationplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       xaxis_param = xaxis_param,
                       xaxis_var = xaxis_var,
                       yaxis_param = yaxis_param,
                       yaxis_var = yaxis_var,
                       trt_group = trt_group,
                       color_manual = color_manual,
                       shape_manual = shape_manual,
                       code_data_processing = code_data_processing
                       ),
    ui = ui_g_correlationplot,
    ui_args = args
  )
  
}

ui_g_correlationplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = div(
      fluidRow(
             uiOutput(ns("plot_ui"))
      ),
      fluidRow(
        column(width = 12,
               br(), hr(),
               h4("Selected Data Points"),
               tableOutput(ns("brush_data"))
        )
      )
    ),
    encoding =  div(
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      optionalSelectInput(ns("xaxis_param"), "Select an X-Axis Biomarker", a$xaxis_param_choices, a$xaxis_param, multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),
      optionalSelectInput(ns("yaxis_param"), "Select a Y-Axis Biomarker", a$yaxis_param_choices, a$yaxis_param, multiple = FALSE),
      optionalSelectInput(ns("yaxis_var"), "Select a Y-Axis Variable", a$yaxis_var_choices, a$yaxis_var, multiple = FALSE),
      radioButtons(ns("constraint_var"), "X-Axis Data Constraint", c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")),
      uiOutput(ns("constraint_min_value"), style="display: inline-block; vertical-align:center"),
      uiOutput(ns("constraint_max_value"), style="display: inline-block; vertical-align:center"),
      tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;"),
      uiOutput(ns("xaxis_zoom")),
      uiOutput(ns("yaxis_zoom")),
      numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
      checkboxInput(ns("facet"), "Treatment Facetting", a$facet),
      checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      numericInput(ns("vline"), "Add a vertical line:", a$vline),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("reg_text_size"), "Regression Annotations Size", a$reg_text_size, ticks = FALSE)
    ),
    # forms = tags$div(
    #   actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
    #   # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    # ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_correlationplot <- function(input, output, session, datasets, dataname, 
                              param_var, xaxis_param, xaxis_var, yaxis_param, yaxis_var, 
                              trt_group, color_manual, shape_manual,
                              code_data_processing) {

  ns <- session$ns

  # filter data to selected params
  filter_ALB <- reactive({
    xaxis_param <- input$xaxis_param
    yaxis_param <- input$yaxis_param
    datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter((eval(parse(text = param_var)) == xaxis_param | eval(parse(text = param_var)) == yaxis_param))
  })  
  
  # create the transformed variable names for ease of reference elsewhere 
  xvar <- reactive(paste0(input$xaxis_var, ".", input$xaxis_param))
  yvar <- reactive(paste0(input$yaxis_var, ".", input$yaxis_param))
  xloqfl <- reactive(paste0("LOQFL_", input$xaxis_param))
  yloqfl <- reactive(paste0("LOQFL_", input$yaxis_param))

  plot_data_transpose <- reactive({
    
    xaxis_var <- input$xaxis_var
    yaxis_var <- input$yaxis_var
    
    # given the 2 param and 2 analysis vars we need to transform the data
    plot_data_t1 <- filter_ALB() %>% gather(ANLVARS, ANLVALS, BASE2, BASE, xaxis_var, yaxis_var, LOQFL) %>%
      mutate(ANL.PARAM = ifelse(ANLVARS == "LOQFL", paste0(ANLVARS, "_", PARAMCD), paste0(ANLVARS, ".", PARAMCD))) %>%
      select(USUBJID, trt_group, AVISITN, AVISITCD, ANL.PARAM, ANLVALS) %>%
      spread(ANL.PARAM, ANLVALS)
    
    # the transformed analysis value variables are character and need to be converted to numeric for ggplot
    # remove records where either of the analysis variables are NA since they will not appear on the plot and
    # will ensure that LOQFL = NA level is removed
    plot_data_t2 <- plot_data_t1 %>%
      subset(!is.na(.[[xvar()]]) & !is.na(.[[yvar()]])) %>%
      mutate_at(vars(contains(".")), as.numeric) %>%
      mutate(LOQFL_COMB = ifelse(.[[xloqfl()]] == "Y" | .[[yloqfl()]] == "Y", "Y", "N"))

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
      
      plot_data_t3 <- plot_data_t2 %>%
        filter(constraint_min_range <= .[[xvar()]] & .[[xvar()]] <= constraint_max_range |
                 is.na(.[[xvar()]])
        )
    } else{
      plot_data_t3 <- plot_data_t2
    }
    
  })
  
  # dynamic plot height and brushing
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(ns("correlationplot"), height = plot_height,
               brush = brushOpts(id = ns("correlationplot_brush"), resetOnNew=T)
               )
    })

  output$brush_data <- renderTable({
    plot_data_t3 <- plot_data_transpose()
    if (nrow(plot_data_t3) > 0 ){
    brushedPoints(select(plot_data_t3, "USUBJID", trt_group, "AVISITCD", xvar(), yvar(), LOQFL_COMB),
                  input$correlationplot_brush)
    } else{
      NULL
    }
      
  })
  
  # dynamic slider for x-axis
  output$xaxis_zoom <- renderUI({
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xaxis_param <- input$xaxis_param
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == xaxis_param)
    
    # establish default value during reaction and prior to value being available
    xmin_scale <- -Inf
    xmax_scale <- Inf
    
    # identify min and max values of BM range ignoring NA values
    xmin_scale <- min(scale_data[[input$xaxis_var]], na.rm = TRUE)
    xmax_scale <- max(scale_data[[input$xaxis_var]], na.rm = TRUE)
    
    tagList({
      sliderInput(ns("xrange_scale"), label="X-Axis Range Zoom", 
                  floor(xmin_scale), ceiling(xmax_scale), 
                  value = c(floor(xmin_scale), ceiling(xmax_scale)))
    })
    
  })

  # dynamic slider for y-axis
  output$yaxis_zoom <- renderUI({
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    yaxis_param <- input$yaxis_param 
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == yaxis_param)
    
    # establish default value during reaction and prior to value being available
    ymin_scale <- -Inf
    ymax_scale <- Inf
    
    # identify min and max values of BM range ignoring NA values
    ymin_scale <- min(scale_data[[input$yaxis_var]], na.rm = TRUE)
    ymax_scale <- max(scale_data[[input$yaxis_var]], na.rm = TRUE)
    
    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Zoom", 
                  floor(ymin_scale), ceiling(ymax_scale), 
                  value = c(floor(ymin_scale), ceiling(ymax_scale)))
    })
    
  })
  
  # minimum data constraint value
  output$constraint_min_value <- renderUI({
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
      validate(need(nrow(ALB) > 0 , "Waiting For Filter Selection"))
      
      xaxis_param <- input$xaxis_param
      scale_data <- ALB %>%
        filter(eval(parse(text = param_var)) == xaxis_param)
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
      validate(need(nrow(ALB) > 0 , "Waiting For Filter Selection"))
      
      xaxis_param <- input$xaxis_param
      scale_data <- ALB %>%
        filter(eval(parse(text = param_var)) == xaxis_param)
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
  
  output$correlationplot <- renderPlot({
    ALB <- filter_ALB()
    xaxis_param <- input$xaxis_param
    xaxis_var <- input$xaxis_var
    yaxis_param <- input$yaxis_param
    yaxis_var <- input$yaxis_var
    xmin_scale <- input$xrange_scale[1]
    xmax_scale <- input$xrange_scale[2]
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    font_size <- input$font_size
    dot_size <- input$dot_size
    reg_text_size <- input$reg_text_size
    hline <- as.numeric(input$hline)
    vline <- as.numeric(input$vline)
    facet_ncol <- input$facet_ncol
    facet <- input$facet
    reg_line <- input$reg_line
    rotate_xlab <- input$rotate_xlab
    
    validate(need(!is.null(ALB) && is.data.frame(ALB), "No data left"))
    validate(need(nrow(ALB) > 0 , "ALB Data No Observations Left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(xaxis_param %in% unique(ALB[[param_var]]),
                  paste("X-Axis Biomarker", xaxis_param, " is not available in data", dataname)))
    validate(need(yaxis_param %in% unique(ALB[[param_var]]),
                  paste("Y-Axis Biomarker", yaxis_param, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ALB),
                  paste("Variable", trt_group, " is not available in data", dataname)))
    validate(need(xaxis_var %in% names(ALB),
                  paste("Variable", xaxis_var, " is not available in data", dataname)))
    validate(need(yaxis_var %in% names(ALB),
                  paste("Variable", yaxis_var, " is not available in data", dataname)))
    
    param_lookup <- unique(ALB[c("PARAMCD", "PARAM")])
    unit_lookup <- unique(ALB[c("PARAMCD", "AVALU")])
    lookups <- inner_join(param_lookup, unit_lookup, by=c("PARAMCD"))

    xparam_meta <- lookups %>%
      filter(PARAMCD == xaxis_param)
    xparam <- xparam_meta$PARAM
    xunit <- xparam_meta$AVALU

    yparam_meta <- lookups %>%
      filter(PARAMCD == yaxis_param)
    yparam <- yparam_meta$PARAM
    yunit <- yparam_meta$AVALU

    # setup the ggtitle label.  Combine the biomarker and the units (if available)
    title_text <- ifelse(is.null(ALB$AVALU), paste(xparam, "and", yparam, "@ Visits"),
                           ifelse(ALB[["AVALU"]] == "", paste(xparam, "and", yparam, "@ Visits"),
                                  paste0(xparam, " (", xunit,") and ", yparam,  " (", yunit,") @ Visits"))
    )

    # setup the x-axis label.  Combine the biomarker and the units (if available)
    xaxis_lab <- ifelse(is.null(ALB$AVALU), paste(xparam, xaxis_var, "Values"),
                         ifelse(ALB[["AVALU"]] == "", paste(xparam, xaxis_var, "Values"),
                                paste0(xparam," (", xunit, ") ", xaxis_var, " Values"))
    )

    # setup the y-axis label.  Combine the biomarker and the units (if available)
    yaxis_lab <- ifelse(is.null(ALB$AVALU), paste(yparam, yaxis_var, "Values"),
                         ifelse(ALB[["AVALU"]] == "", paste(yparam, yaxis_var, "Values"),
                                paste0(yparam," (", yunit,") ", yaxis_var, " Values"))
    )

    plot_data_t3 <- plot_data_transpose()
    validate(need(nrow(plot_data_t3) > 0 , "Plot Data No Observations Left"))
    
    # re-establish treatment variable label
    if (trt_group == "ARM"){
      attributes(plot_data_t3$ARM)$label <- "Planned Arm"
    } else {
      attributes(plot_data_t3$ACTARM)$label <- "Actual Arm"
    }
    
    
    p <- goshawk::g_correlationplot(
      data = plot_data_t3,
      param_var = param_var,
      xaxis_param = xaxis_param,
      xaxis_var = xaxis_var,
      xvar = xvar(),
      yaxis_param = yaxis_param,
      yaxis_var = yaxis_var,
      yvar = yvar(),
      trt_group = trt_group,
      xmin = xmin_scale,
      xmax = xmax_scale,
      ymin = ymin_scale,
      ymax = ymax_scale,
      title_text = title_text,
      xaxis_lab = xaxis_lab,
      yaxis_lab = yaxis_lab,
      color_manual = color_manual,
      shape_manual = shape_manual,
      facet_ncol = facet_ncol,
      facet = facet,
      reg_line = reg_line,
      font_size = font_size,
      dot_size = dot_size,
      reg_text_size = reg_text_size,
      rotate_xlab = rotate_xlab,
      hline = hline,
      vline = vline      
    )

    p

  })
  
}
