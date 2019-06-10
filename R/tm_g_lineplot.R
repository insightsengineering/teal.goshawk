#' Line plot
#' 
#' This teal module renders the UI and calls the function that creates a line plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured laboratory data frame ALB.
#' @param aslname Name of asl data set from which additional variables will be used for shape_choices
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data that includes parameter labels.
#' @param xvar single name of variable in analysis data that is used as x-axis in the plot for the respective goshawk function.
#' @param xvar_choices vector with variable names that can be used as xvar.
#' @param xvar_level vector that can be used to define the factor level of xvar. Only use it when xvar is character or factor.
#' @param filter_var data constraint variable.
#' @param filter_var_choices data constraint variable choices.
#' @param yvar single name of variable in analysis data that is used as summary variable in the respective gshawk function.
#' @param yvar_choices vector with variable names that can be used as yvar.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param shape_choices Vector with names of ASL variables which can be used to change shape
#' @param man_color string vector representing customized colors
#' @param stat string of statistics
#' @param hline numeric value to add horizontal line to plot
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric. Default value is waive().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values. Default value is waive().
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels.
#' @param plot_height numeric vectors to define the plot height.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control the position dodge of error bar
#' 
#' @import goshawk
#'
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return \code{shiny} object
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # EXAMPLE
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam('ASL', N = 100)
#' ANL <- expand.grid(
#'   STUDYID = "STUDY A",
#'   USUBJID = paste0("id-",1:100),
#'   VISITN = c(1:10),
#'   ARM = c("ARM A", "ARM B"),
#'   PARAMCD = c("CRP", "IGG", "IGM")
#' )
#' ANL$VISIT <- paste0("visit ", ANL$VISITN)
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$AVALU <- "mg"
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' ANL$PCHG <- ANL$CHG/ANL$AVAL*100
#' ANL$PARAM <- ANL$PARAMCD
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ANL),
#'   modules = root_modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ALB",
#'       aslname = "ASL",
#'       param_var = "PARAMCD",
#'       param_choices = c("CRP","IGG","IGM"),
#'       shape_choices = c("SEX", "RACE"),
#'       param = "CRP",
#'       xvar = "VISIT",
#'       yvar = "AVAL",
#'       yvar_choices = c("AVAL","CHG","PCGH"),
#'       trt_group = "ARM"
#'     )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)
#' }

tm_g_lineplot <- function(label,
                          dataname,
                          aslname  = NULL,
                          param_var,
                          param_choices = param,
                          param,
                          param_var_label = 'PARAM',
                          xvar, yvar,
                          xvar_choices = xvar, yvar_choices = yvar,
                          xvar_level = NULL,
                          filter_var = yvar,
                          filter_var_choices = filter_var,
                          trt_group,
                          trt_group_level = NULL,
                          shape_choices = NULL,
                          stat = "mean",
                          hline = NULL,
                          man_color = NULL,
                          xtick = waiver(), xlabel = xtick,
                          rotate_xlab = FALSE,
                          plot_height = c(600, 200, 2000),
                          font_size = c(12, 8, 20),
                          dodge = c(0.4, 0, 1)) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_lineplot,
    server_args = list(dataname = dataname, aslname = aslname, param_var = param_var, trt_group = trt_group, man_color = man_color,
                       xvar_level = xvar_level, trt_group_level = trt_group_level, shape_choices = shape_choices, param_var_label = param_var_label,
                       xtick = xtick, xlabel = xlabel),
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
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xvar"), "X-Axis Variable", a$xvar_choices, a$xvar, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "Select a Y-Axis Variable", a$yvar_choices, a$yvar, multiple = FALSE),
      uiOutput(ns("shape_ui")),
      radioButtons(ns("stat"), "Select a Statistic:", c("mean","median"), a$stat),
      radioButtons(ns("filter_var"), "Data Constraint", a$filter_var_choices, a$filter_var),
      uiOutput(ns("filter_min"), style="display: inline-block; vertical-align:center"),
      uiOutput(ns("filter_max"), style="display: inline-block; vertical-align:center"),
      uiOutput(ns("yaxis_scale")),
      
      if (all(c(
        length(a$plot_height) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;")
      },
      checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
      div(style="padding: 0px;",
          div(style="display: inline-block;vertical-align:moddle; width: 175px;",
              tags$b("Add a Horizontal Line:")),
          div(style="display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("hline"), "", a$hline))
      ),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("dodge"), "Error Bar Position Dodge", a$dodge, ticks = FALSE),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE)
    )
    # ,
    # forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
  
}

srv_lineplot <- function(input, output, session, datasets, dataname, aslname, param_var, trt_group, man_color, xvar_level, 
                         trt_group_level, shape_choices, param_var_label, xtick, xlabel) {
  
  ns <- session$ns
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("lineplot"), height=plot_height)
  })
  
  output$shape_ui <- renderUI({
    if(!is.null(shape_choices)){
      selectInput(ns("shape"), "Select split by line type. Set blank for no split",
                  choices = c("None", shape_choices), selected = "None")
    }
  })
  
  # Filter data based on input filter_var
  observe({
    # derive min max value of input filter_var
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    param <- input$param
    value_var <- input$filter_var
    scale_data <- filter(ANL, eval(parse(text = param_var)) == param)
    
    if(value_var == "NONE"){
      output$filter_max <- NULL
      output$filter_min <- NULL
      output$filter_val_scale <- NULL
    } else {
      # identify min and max values of BM range ignoring NA values
      min_scale <- min(scale_data[,value_var], na.rm = TRUE)
      max_scale <- max(scale_data[,value_var], na.rm = TRUE)
      
      # Output variable UI
      output$filter_min <- renderUI({
        tagList({
          numericInput(session$ns("filtermin"), label = paste0("Min (", min_scale, ")"), value = min_scale, min = min_scale, max = max_scale)
        })
      })
      
      output$filter_max <- renderUI({
        tagList({
          numericInput(session$ns("filtermax"), label = paste0("Max (", max_scale, ")"), value = max_scale, min = min_scale, max = max_scale)
        })
      })
      
      output$filter_val_scale <- renderUI({
        tagList({
          sliderInput(ns("filter_scale"), label=paste0("Select Data for ", value_var), 
                      floor(min_scale), ceiling(max_scale),
                      value = c(floor(min_scale), ceiling(max_scale)))
        })
      })
    }
  })


  # filter data by param and the y-axis range values
  filter_ANL <- reactive({

    param <- input$param
    filter_var <- input$filter_var
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter(eval(parse(text = param_var)) == param)
    
    if (!is.null(aslname)){
      ASL <- datasets$get_data(aslname, filtered = TRUE, reactive = TRUE)
    }
    if(!is.null(shape_choices)){
      validate(need(aslname, "aslname must be specified when shape_choices is not NULL"))
      validate(need(all(shape_choices%in%names(ASL)), "shape_choices must be contained in asl!"))
      ANL <- left_join(
        ANL, 
        select(ASL, c("STUDYID", "USUBJID",shape_choices)),
        by = c("STUDYID", "USUBJID")
      )
    }

    ymin_scale <- -Inf
    ymax_scale <- Inf
    
    if(filter_var != "NONE"){
      if (length(input$filtermin)){
        ymin_scale <- input$filtermin
      }
      
      if (length(input$filtermax)){
        ymax_scale <- input$filtermax
      }
      
      ANL1 <- ANL %>%
        filter((ymin_scale <= eval(parse(text = filter_var)) &
                  eval(parse(text = filter_var)) <= ymax_scale) |
                 (is.na(filter_var)))
      
      return(ANL1)
    } else {
      return(ANL)
    }
      
      
  })
  
  # dynamic slider for y-axis
  output$yaxis_scale <- renderUI({
    xvar <- input$xvar
    value_var <- input$yvar
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    ANL <- filter_ANL() 

    scale_data <- ANL %>%
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
      ymax_scale <- max(scale_data[,c('median','quant25','quant75')], na.rm = TRUE)
    } else {
      ymin_scale <- min(scale_data[,c('mean','CIup','CIdown')], na.rm = TRUE)
      ymax_scale <- max(scale_data[,c('mean','CIup','CIdown')], na.rm = TRUE)
    }
    
    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Zoom", 
                  round(ymin_scale, digits = 1), round(ymax_scale, digits = 1), 
                  value = c(round(ymin_scale, digits = 1), round(ymax_scale, digits = 1)))
    })
    
  })
  
  
  chunks <- list(
    analysis = "# Not Calculated"
  )
  
  plotout <- reactive({
    
    ANL <- filter_ANL()
    param <- input$param
    xvar <- input$xvar
    yvar <- input$yvar
    ylim <- input$yrange_scale
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    rotate_xlab <- input$rotate_xlab
    hline <- as.numeric(input$hline)
    font_size <- input$font_size
    dodge <- input$dodge
    height <- input$plot_height
   
    
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
    
    # re-establish treatment variable label
    if (trt_group == "ARM"){
      attributes(ANL$ARM)$label <- "Planned Arm"
    } else {
      attributes(ANL$ACTARM)$label <- "Actual Arm"
    }
    
    shape <- NULL
    if (!is.null(input$shape)){
      if (input$shape != "None"){
        shape <- input$shape
      }
    }
    chunks$analysis <<- call(
      "g_lineplot",
      data = bquote(.(as.name(data_name))),
      biomarker_var = param_var,
      biomarker_var_label = param_var_label,
      biomarker = param,
      value_var = yvar,
      ylim = ylim,
      trt_group = trt_group,
      trt_group_level = trt_group_level,
      shape = shape,
      time = xvar,
      time_level = xvar_level,
      color_manual = man_color,
      median = median,
      hline = hline,
      xtick = xtick,
      xlabel = xlabel,
      rotate_xlab = rotate_xlab,
      font_size = font_size,
      dodge = dodge,
      plot_height = height
    )

    p <- try(eval(chunks$analysis))

    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create the line plot:\n\n", p)))
    
    p
    
  })
  
  output$lineplot <- renderPlot({
    plotout()

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
