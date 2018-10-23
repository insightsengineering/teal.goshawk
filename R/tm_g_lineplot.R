#' Line plot Teal Module
#' 
#'
#' @param label menue item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in the list passed to the data argument of init. 
#' Note that the data is expected to be in vertical form with the PARAMCD variable filtering to one observation per patient.
#' @param xvar single name of variable in analysis data that is used as x-axis in the plot for the respective goshawk function.
#' @param xvar_choices vector with variable names that can be used as xvar.
#' @param xvar_level vector that can be used to define the factor level of xvar.
#' @param yvar single name of variable in analysis data that is used as summary variable in the respective gshawk function.
#' @param yvar_choices vector with variable names that can be used as yvar.
#' @param param_var single name of variable in analysis data that includes parameter names.
#' @param param_var_label single name of variable in analysis data that includes parameter lables.
#' @param param parameter name
#' @param param_choices vector of parameter names that can be used in param.
#' @param trt_group single name of treatment arm variable.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param man_color string vector representing customized colors
#' @param stat string of statistics
#' @param hline numeric value to add horizontal line to plot
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels
#' @param plot_height numeric vectors to define the plot height.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control the position dodge of error bar
#' 
#' 
#' @import goshawk
#'
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details 
#'
#' @return \code{shiny} object
#'
#' @export
#'
#' @examples
#' 
#' # EXAMPLE
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam('ASL', N = 100)
#' ANL <- expand.grid(
#'   STUDYID = "STUDY A",
#'   USUBJID = paste0("id-",1:100),
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B"),
#'   PARAMCD = c("CRP", "IGG", "IGM")
#' )
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' ANL$PCHG <- ANL$CHG/ANL$AVAL*100
#' 
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ANL),
#'   modules = root_modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ALB",
#'       xvar = "VISIT",
#'       yvar = "AVAL",
#'       yvar_choices = c("AVAL","CHG","PCGH"),
#'       param_var = "PARAMCD",
#'       param = "CRP",
#'       param_choices = c("CRP","IGG","IGM"),
#'       trt_group = "ARM"
#'     )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)


tm_g_lineplot <- function(label,
                          dataname,
                          xvar, yvar,
                          xvar_choices = xvar, yvar_choices = yvar,
                          xvar_level = NULL,
                          param_var, param_var_label = 'PARAM',
                          param, param_choices = param,
                          trt_group,
                          trt_group_level = NULL,
                          stat = "mean",
                          hline = NULL,
                          man_color = NULL,
                          rotate_xlab = FALSE,
                          plot_height = c(600, 200, 2000),
                          font_size = c(12, 8, 20),
                          dodge = c(0.4, 0, 1)) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_lineplot,
    server_args = list(dataname = dataname, param_var = param_var, trt_group = trt_group, man_color = man_color,
                       xvar_level = xvar_level, trt_group_level = trt_group_level, param_var_label = param_var_label),
    ui = ui_lineplot,
    ui_args = args,
    filters = dataname
  )
  
}


ui_lineplot <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  if (a$plot_height < 200 || a$plot_height > 2000) stop("plot_height must be between 200 and 2000")
  
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      helpText("Biomarker parameter variable:", tags$code(a$param_var)),
      optionalSelectInput(ns("param"), "Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xvar"), "x variable", a$xvar_choices, a$xvar, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable", a$yvar_choices, a$yvar, multiple = FALSE),
      helpText("Treatment group:", tags$code(a$trt_group)),
      radioButtons(ns("stat"), "Select statistics:", c("mean","median"), a$stat),

      if (all(c(
        length(a$plot_height) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;")
      },
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      uiOutput(ns("yaxis_scale")),
      uiOutput(ns("yvar_scale")),
      optionalSliderInputValMinMax(ns("dodge"), "Error bar position dodge", a$dodge, ticks = FALSE),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
  
}

srv_lineplot <- function(input, output, session, datasets, dataname, param_var, trt_group, man_color, xvar_level, trt_group_level, param_var_label) {
  
  ns <- session$ns
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("lineplot"), height=plot_height)
  })
  
  # dynamic slider for y-axis
  output$yaxis_scale <- renderUI({
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    param <- input$param 
    xvar <- input$xvar
    value_var <- input$yvar
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    
    scale_data <- ANL %>%
      filter(eval(parse(text = param_var)) == param) %>%
      group_by(eval(parse(text = xvar)),
               eval(parse(text = trt_group))) %>%
      summarise(mean = mean(eval(parse(text = value_var)),na.rm = TRUE),
                CIup = mean(eval(parse(text = value_var)),na.rm = TRUE) + 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
                CIdown = mean(eval(parse(text = value_var)),na.rm = TRUE) - 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
                median = median(eval(parse(text = value_var)),na.rm = TRUE),
                quant25 = quantile(eval(parse(text = value_var)), 0.25, na.rm = TRUE),
                quant75 = quantile(eval(parse(text = value_var)), 0.75, na.rm = TRUE))
    
    # identify min and max values of BM range ignoring NA values
    ymin_scale <- -Inf
    ymax_scale <- Inf
    
    if(median){
      ymin_scale <- min(scale_data[,c('median','quant25','quant75')], na.rm = TRUE)
      ymax_scale <- max(scale_data[,c('quant25','quant75')], na.rm = TRUE)
    } else {
      ymin_scale <- min(scale_data[,c('mean','CIup','CIdown')], na.rm = TRUE)
      ymax_scale <- max(scale_data[,c('CIup','CIdown')], na.rm = TRUE)
    }
    
    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Scale", 
                  round(ymin_scale*1.1, digits = 1), round(ymax_scale*1.1, digits = 1), 
                  value = c(round(ymin_scale*1.1, digits = 1), round(ymax_scale*1.1, digits = 1)))
    })
    
  })
  
  # dynamic slider for filter input value by parameter
  output$yvar_scale <- renderUI({
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    param <- input$param
    value_var <- input$yvar
    scale_data <- filter(ANL, eval(parse(text = param_var)) == param)

    # identify min and max values of BM range ignoring NA values
    ymin_scale <- min(scale_data[,value_var], na.rm = TRUE)
    ymax_scale <- max(scale_data[,value_var], na.rm = TRUE)

    tagList({
      sliderInput(ns("yfilter_scale"), label=paste0(value_var, " Value Range Scale"), 
                  floor(ymin_scale), ceiling(ymax_scale),
                  value = c(floor(ymin_scale), ceiling(ymax_scale)))
    })
  })


  # filter data by param and the y-axis range values
  filter_ANL <- reactive({

    param <- input$param
    yvar <- input$yvar
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    ymin_scale <- -Inf
    ymax_scale <- Inf

    if (length(input$yfilter_scale)){
      ymin_scale <- input$yfilter_scale[1]
      ymax_scale <- input$yfilter_scale[2]
    }

    ANL %>%
      filter(eval(parse(text = param_var)) == param &
               (ymin_scale <= eval(parse(text = yvar)) &
                  eval(parse(text = yvar)) <= ymax_scale) |
               (is.na(yvar)))
  })
  
  chunks <- list(
    analysis = "# Not Calculated"
  )
  
  output$lineplot <- renderPlot({
    
    ANL <- filter_ANL()
    param <- input$param
    xvar <- input$xvar
    yvar <- input$yvar
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    rotate_xlab <- input$rotate_xlab
    hline <- as.numeric(input$hline)
    font_size <- input$font_size
    dodge <- input$dodge
    
    chunks$analysis <<- "# Not Calculated"
    
    validate(need(!is.null(ANL) && is.data.frame(ANL), "no data left"))
    validate(need(nrow(ANL) > 0 , "no observations left"))
    validate(need(param_var %in% names(ANL),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ANL[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(xvar %in% names(ANL),
                  paste("variable", xvar, " is not available in data", dataname)))
    validate(need(yvar %in% names(ANL),
                  paste("variable", yvar, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ANL),
                  paste("variable", trt_group, " is not available in data", dataname)))
    
    
    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, ANL)

    chunks$analysis <<- call(
      "g_lineplot",
      data = bquote(.(as.name(data_name))),
      biomarker_var = param_var,
      biomarker_var_label = param_var_label,
      biomarker = param,
      value_var = yvar,
      ymin = ymin_scale,
      ymax = ymax_scale,
      trt_group = trt_group,
      trt_group_level = trt_group_level,
      time = xvar,
      time_level = xvar_level,
      color_manual = man_color,
      median = median,
      hline = hline,
      rotate_xlab = rotate_xlab,
      font_size = font_size,
      dodge = dodge
    )

    p <- try(eval(chunks$analysis))

    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create the line plot:\n\n", p)))
    
    p
    
  })
  
  observeEvent(input$show_rcode, {
    
    header <- teal.tern:::get_rcode_header(
      title = "Line Plot",
      datanames = dataname,
      datasets = datasets
    )
    
    str_rcode <- paste(c(
      "",
      header,
      "",
      teal.tern:::remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")
    
    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Line Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}
