#' Scatter Plot
#'
#' This teal module renders the UI and calls the function that creates a scatter plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured 
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. BASE.
#' @param xaxis_var_choices list of variables containing biomarker results choices.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL.
#' @param yaxis_var_choices list of variables containing biomarker results choices.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in 
#' visualization. Use with facet TRUE.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#' @param code_data_processing TODO
#'
#' @inheritParams teal.devel::standard_layout
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @import DescTools
#' @import dplyr
#' @import goshawk
#' @import teal
#' @import teal.devel
#'
#' @details This module displays a scatter plot. link to specification file \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(goshawk)
#' library(random.cdisc.data)
#' library(stringr)
#' library(teal)
#' library(teal.devel)
#' library(teal.goshawk)
#' 
#' # code>
#' 
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", 
#'                     "C: Combination" = "Combination")
#' color_manual <-  c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <-  c("N"  = 1, "Y"  = 2, "NA" = 0)
#' 
#' ADSL <- radsl(N = 20, seed = 1)
#' ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#' ADLB <- ADLB %>% 
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL", grepl("WEEK", AVISIT) ~ paste("W",trimws(substr(AVISIT, start=6, 
#'                                                                                  stop=str_locate(AVISIT, "DAY")-1))),
#'     TRUE ~ as.character(NA))) %>%
#'   mutate(AVISITCDN = case_when(AVISITCD == "SCR" ~ -2,
#'                                AVISITCD == "BL" ~ 0, grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)), 
#'                                TRUE ~ as.numeric(NA))) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'                          ifelse(grepl("B", ARMCD), 2,
#'                                 ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>% reorder(TRTORD))
#' 
#' # <code
#' 
#' param_choices = c("ALT", "CRP", "IGA")
#' 
#' x <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'        label = "Scatter Plot",
#'        dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        param_choices = param_choices,
#'        param = param_choices[1],
#'        xaxis_var = "BASE",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "AVALL2"),
#'        yaxis_var = "AVAL",
#'        yaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "AVALL2"),
#'        trt_group = "ARM",
#'        color_manual = color_manual,
#'        shape_manual = shape_manual,
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
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_scatterplot <- function(label,
                             dataname,
                             param_var,
                             param,
                             param_choices = param,
                             xaxis_var,
                             xaxis_var_choices = xaxis_var,
                             yaxis_var, 
                             yaxis_var_choices = yaxis_var,
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
                             post_output = NULL,
                             code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_scatterplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       param = param,
                       xaxis_var = xaxis_var,
                       yaxis_var = yaxis_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual,
                       code_data_processing = code_data_processing
    ),
    ui = ui_g_scatterplot,
    ui_args = args
  )
  
}

ui_g_scatterplot <- function(id, ...) {
  
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
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),
      optionalSelectInput(ns("yaxis_var"), "Select a Y-Axis Variable", a$yaxis_var_choices, a$yaxis_var, multiple = FALSE),
      radioButtons(ns("constraint_var"), "Data Constraint", c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")),
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
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_g_scatterplot <- function(input, output, session, datasets, dataname, 
                              param_var, param, xaxis_var, yaxis_var, 
                              trt_group, facet_var, color_manual, shape_manual,
                              code_data_processing) {
  
  # initialize the code chunk container in the app environment
  init_chunks()
  
  dataset_var <- paste0(dataname, "_FILTERED")
  filter_ANL <- reactiveValues(data = NULL)
  
  
  output$scatterplot <- renderPlot({
    
    # capture data in current filtered state as reflected in right hand panel filter
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    
    # assign input values to local reactive environment
    param <- input$param
    constraint_var <- input$constraint_var
    constraint_min <- input$constraint_min
    constraint_max <- input$constraint_max
    
    # create code chunk "ANL" in chunk container 
    anl_call <- if (constraint_var != "NONE") {
      
      constraint_min_range <- -Inf
      constraint_max_range <- Inf
      
      if (length(constraint_min)) {
        constraint_min_range <- constraint_min
      }
      
      if (length(constraint_max)) {
        constraint_max_range <- constraint_max
      }
      
      # add code chunk to chunk container
      bquote({
        ANL <- .(as.name(dataset_var)) %>%
          filter(
            .(as.name(param_var)) == .(param) &
              .(constraint_min_range) <= .(as.name(constraint_var)) &
              .(as.name(constraint_var)) <= .(constraint_max_range) |
              is.na(.(as.name(constraint_var)))
          )
      })
      
    } else {
      # add code chunk to chunk container
      bquote({
        ANL <- .(as.name(dataset_var)) %>%
          filter(.(as.name(param_var)) == .(param))
      })
    }
    
    
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)))
    chunks_push(anl_call)
    
    ANL <- chunks_get_var("ANL")
    filter_ANL$data <- ANL # to trigger other reactive endpoints
    
    # validate resulting data object for minimum requirements
    validate(need(!is.null(ANL) && is.data.frame(ANL), 
                  paste("Analysis dataset (ANL) is not available in app environment.")))
    validate(need(nrow(ANL) > 0, 
                  "Minimum number of records not met: > 0 records required."))
    
    
    chunks_push(bquote({
      param_var <- .(param_var)
      trt_group <- .(trt_group)
    }))
    
    param <- input$param
    xaxis_var <- input$xaxis_var
    yaxis_var <- input$yaxis_var
    color_manual <- color_manual
    shape_manual <- shape_manual
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
    facet_var <- facet_var
    reg_line <- input$reg_line
    rotate_xlab <- input$rotate_xlab
    
    chunks_push(bquote({
      # re-establish treatment variable label
      if (trt_group == "ARM"){
        attributes(ANL$ARM)$label <- "Planned Arm"
      } else {
        attributes(ANL$ACTARM)$label <- "Actual Arm"
      }
      
      p <- g_scatterplot(
        data = ANL,
        param_var = .(param_var),
        param = .(param),
        xaxis_var = .(xaxis_var),
        yaxis_var = .(yaxis_var),
        trt_group = .(trt_group),
        xmin = .(xmin_scale),
        xmax = .(xmax_scale),
        ymin = .(ymin_scale),
        ymax = .(ymax_scale),
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
        hline = .(hline),
        vline = .(vline)      
      )
      
      p
      
    }))
    
    # evaluate the code chunk so that it is available in app environment as well
    chunks_safe_eval()
    
  })
  
  ns <- session$ns
  # dynamic plot height and brushing
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(ns("scatterplot"), height = plot_height,
               brush = brushOpts(id = ns("scatterplot_brush"), resetOnNew=T)
    )
  })
  
  output$brush_data <- renderTable({
    ANL <- filter_ANL$data
    if (!is.null(ANL) && nrow(ANL) > 0 ){
      brushedPoints(select(ANL,"USUBJID", trt_group, "AVISITCD", "PARAMCD", input$xaxis_var, input$yaxis_var, "LOQFL"), input$scatterplot_brush)
    } else{
      NULL
    }
  })
  
  # dynamic slider for x-axis
  output$xaxis_zoom <- renderUI({
    ANL <- filter_ANL$data
    
    if (is.null(ANL)) {
      return(NULL)
    }
    
    param <- input$param 
    
    scale_data <- ANL %>%
      filter(eval(parse(text = param_var)) == param)
    
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
    ANL <- filter_ANL$data
    if (is.null(ANL)) {
      return(NULL)
    }
    
    
    param <- input$param 
    
    scale_data <- ANL %>%
      filter(eval(parse(text = param_var)) == param)
    
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
    ANL <- filter_ANL$data
    
    if (is.null(ANL)) {
      return(NULL)
    }
    
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      validate(need(nrow(ANL) > 0 , "Waiting For Filter Selection"))
      param <- input$param
      scale_data <- ANL %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint variable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & any(grepl("SCR", visitFreq)) | 
          input$constraint_var == "BASE" & any(grepl("BL", visitFreq))){
        # identify min and max values of constraint var range ignoring NA values
        constraint_min_range <- round(min(scale_data[[input$constraint_var]], na.rm = TRUE), 3)
        constraint_max_range <- round(max(scale_data[[input$constraint_var]], na.rm = TRUE), 3)
        tagList({
          numericInput(ns("constraint_min"), 
                       paste0("Min (", constraint_min_range, ")"), 
                       value = constraint_min_range, 
                       min = constraint_min_range, max = constraint_max_range)
        })
      }
    } else {
      return(NULL)
    }
    
  })
  
  # maximum data constraint value
  output$constraint_max_value <- renderUI({
    ANL <- filter_ANL$data
    
    if (is.null(ANL)) {
      return(NULL)
    }
    
    
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      validate(need(nrow(ANL) > 0 , "Waiting For Filter Selection"))
      param <- input$param
      scale_data <- ANL %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint variable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & any(grepl("SCR", visitFreq)) | 
          input$constraint_var == "BASE" & any(grepl("BL", visitFreq))){
        # identify min and max values of constraint var range ignoring NA values
        constraint_min_range <- round(min(scale_data[[input$constraint_var]], na.rm = TRUE), 3)
        constraint_max_range <- round(max(scale_data[[input$constraint_var]], na.rm = TRUE), 3)
        tagList({
          numericInput(ns("constraint_max"), 
                       paste0("Max (", constraint_max_range, ")"),
                       value = constraint_max_range, 
                       min = constraint_min_range, max = constraint_max_range)
        })
      }
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Scatter Plot", 
      rcode = get_rcode(
        datasets = datasets,
        title = "Scatter Plot"
      )
    )
  })
  
}
