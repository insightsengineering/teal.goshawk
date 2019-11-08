#' Density Distribution Plot
#'
#' This teal module renders the UI and calls the functions that create a density distribution plot 
#' and an accompanying summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured 
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. AVAL.
#' @param xaxis_var_choices list of variables containing biomarker results choices.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param color_comb name or hex value for combined treatment color.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param hline y-axis value to position a horizontal line.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param code_data_processing TODO
#'
#' @inheritParams teal.devel::standard_layout
#' 
#' @import DescTools
#' @import dplyr
#' @import goshawk
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
#'
#' # Example using ADaM structure analysis dataset.
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(goshawk)
#' library(random.cdisc.data)
#' library(stringr)
#' library(teal)
#' library(teal.goshawk)
#'  
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", 
#' "C: Combination" = "Combination")
#' color_manual <-  c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <-  c("N"  = 1, "Y"  = 2, "NA" = 0)
#' 
#' ADSL <- radsl(N = 20, seed = 1)
#' ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7, seed = 2)
#' ADLB <- ADLB %>% 
#' mutate(AVISITCD = case_when(
#' AVISIT == "SCREENING" ~ "SCR",
#' AVISIT == "BASELINE" ~ "BL", grepl("WEEK", AVISIT) ~ paste("W",trimws(substr(AVISIT, start=6, 
#' stop=str_locate(AVISIT, "DAY")-1))),
#' TRUE ~ as.character(NA))) %>%
#' mutate(AVISITCDN = case_when(AVISITCD == "SCR" ~ -2,
#' AVISITCD == "BL" ~ 0, grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)), 
#' TRUE ~ as.numeric(NA))) %>%
#' # use ARMCD values to order treatment in visualization legend
#' mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#' ifelse(grepl("B", ARMCD), 2,
#' ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#' mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#' mutate(ARM = factor(ARM) %>% reorder(TRTORD))
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
#'     tm_g_density_distribution_plot(
#'        label = "Density Distribution Plot",
#'        dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        param_choices = param_choices,
#'        param = param_choices[1],
#'        xaxis_var = "AVAL",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "AVALL2"),
#'        trt_group = "ARM",
#'        color_manual = color_manual,
#'        color_comb = "#39ff14",
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
                                           color_comb = NULL,
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
                       color_comb = color_comb,
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
    output = div(
      fluidRow(
        uiOutput(ns("plot_ui"))
      ),
      fluidRow(
        column(width = 12,
               br(), hr(),
               h4("Descriptive Statistics"),
               uiOutput(ns("table_ui"))
        )
      )
    ),
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
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_g_density_distribution_plot <- function(input, output, session, datasets, dataname, param_var, param, 
                                            xaxis_var, trt_group, color_manual, color_comb, code_data_processing) {
  
  ns <- session$ns # must add for the dynamic ui.range_scale field
  # initialize the code chunk container in the app environment
  init_chunks()
  dataset_var <- paste0(dataname, "_FILTERED")
  
  # create a reactive data object 
  filter_ANL <- reactive({
    
    # capture data in current filtered state as reflected in right hand panel filter
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    
    # assign input values to local reactive environment
    param <- input$param
    constraint_var <- input$constraint_var
    constraint_min <- input$constraint_min
    constraint_max <- input$constraint_max
    
    # clear code chunk container and prime container with ADLB_FILTERED data object
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)))
    
    # create code chunk "ANL" in chunk container 
    if (constraint_var != "NONE") {
      
      constraint_min_range <- -Inf
      constraint_max_range <- Inf
      
      if (length(constraint_min)) {
        constraint_min_range <- constraint_min
      }
      
      if (length(constraint_max)) {
        constraint_max_range <- constraint_max
      }
      
      # add code chunk to chunk container
      chunks_push(bquote({
        ANL <- .(as.name(dataset_var)) %>%
          filter(
            .(as.name(param_var)) == .(param) &
              .(constraint_min_range) <= .(as.name(constraint_var)) &
              .(as.name(constraint_var)) <= .(constraint_max_range) |
              is.na(.(as.name(constraint_var)))
          )
      }))
      
    } else {
      # add code chunk to chunk container
      chunks_push(bquote({
        ANL <- .(as.name(dataset_var)) %>%
          filter(.(as.name(param_var)) == .(param))
      }))
    }
    
    # evaluate unevaluated code chunks in container
    chunks_safe_eval()
    
    # run code stored in "encoding_filter" code chunk
    ANL <- chunks_get_var("ANL")
    
    
    # validate resulting data object for minimum requirements
    validate(need(!is.null(ANL) && is.data.frame(ANL), 
                  paste("Analysis dataset (ANL) is not available in app environment.")))
    validate(need(nrow(ANL) > 0, 
                  "Minimum number of records not met: > 0 records required."))
    
    # ensure ANL data object is returned by this reactive block
    ANL
    
  })
  
  # # filter data by param and the constraint_min and constraint_max values
  # filter_ALB <- reactive({
  #   param <- input$param
  #   constraint_var <- input$constraint_var
  #   
  #   if (constraint_var != "NONE"){
  #     constraint_min_range <- -Inf
  #     constraint_max_range <- Inf
  #     
  #     if (length(input$constraint_min)){
  #       constraint_min_range <- input$constraint_min
  #     }
  #     
  #     if (length(input$constraint_max)){
  #       constraint_max_range <- input$constraint_max
  #     }
  #     datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
  #       filter(eval(parse(text = param_var)) == param &
  #                constraint_min_range <= eval(parse(text = constraint_var)) &
  #                eval(parse(text = constraint_var)) <= constraint_max_range |
  #                is.na(constraint_var)
  #       )
  #   } else{
  #     datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
  #       filter(eval(parse(text = param_var)) == param)
  #   }
  # })
  
  output$density_distribution_plot <- renderPlot({
    
    # clears code chunks and creates ANL data object in local renderPlot environment
    ANL <- filter_ANL()
    
    chunks_push(bquote({
      param_var <- .(param_var)
      trt_group <- .(trt_group)
    }))
    
    param <- input$param
    xaxis_var <- input$xaxis_var
    xmin_scale <- input$xrange_scale[1]
    xmax_scale <- input$xrange_scale[2]
    font_size <- input$font_size
    line_size <- input$line_size
    hline <- as.numeric(input$hline)
    facet_ncol <- input$facet_ncol
    rotate_xlab <- input$rotate_xlab
    
    chunks_push(bquote({
    # re-establish treatment variable label
    if (trt_group == "ARM"){
      attributes(ANL$ARM)$label <- "Planned Arm"
    } else {
      attributes(ANL$ACTARM)$label <- "Actual Arm"
    }
    
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
      rotate_xlab = .(rotate_xlab),
      hline = .(hline)
    )
    
    p
    
    }))
    
    # evaluate the code chunk so that it is available in app environment as well
    chunks_safe_eval()
    
  })
  
  output$table_ui <- renderTable({
    
    # clears code chunks and creates ANL data object in local renderPlot environment
    ANL <- filter_ANL()
    
    chunks_push(bquote({
      param_var <- .(param_var)
      trt_group <- .(trt_group)
    }))
    
    param <- input$param
    xaxis_var <- input$xaxis_var
    font_size <- input$font_size
    
    chunks_push(bquote({
    t <- t_summarytable(
      data = ANL,
      trt_group = .(trt_group),
      param_var = .(param_var),
      param = .(param),
      xaxis_var = .(xaxis_var),
      font_size = .(font_size)
    )
    
    t
    
    }))
    
    # evaluate the code chunk so that it is available in app environment as well
    chunks_safe_eval()
    
  })
  
  # dynamic plot width
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("density_distribution_plot"), height = plot_height)
  })
  
  # dynamic slider for x-axis
  output$xaxis_zoom <- renderUI({
    ANL <- filter_ANL()
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
  
  # minimum data constraint value
  output$constraint_min_value <- renderUI({
    ANL <- filter_ANL()
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      validate(need(nrow(ANL) > 0 , "Waiting For Filter Selection"))
      param <- input$param
      scale_data <- ANL %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint vatriable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & any(grepl("SCR", visitFreq)) | 
          input$constraint_var == "BASE" & any(grepl("BL", visitFreq))){
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
    ANL <- filter_ANL()
    # conditionally reveal min and max constraint fields
    if (input$constraint_var != "NONE") {
      validate(need(nrow(ANL) > 0 , "Waiting For Filter Selection"))
      
      param <- input$param
      scale_data <- ANL %>%
        filter(eval(parse(text = param_var)) == param)
      # ensure that there are records at visit to process based on the constraint vatriable selection
      visitFreq <- unique(scale_data$AVISITCD)
      if (input$constraint_var == "BASE2" & any(grepl("SCR", visitFreq)) | 
          input$constraint_var == "BASE" & any(grepl("BL", visitFreq))){
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
