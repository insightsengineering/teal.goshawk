#' Teal Module: box plot
#'
#' This shiny module displays a box plot
#'
#' @param label menu item label of the module in the teal app 
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data are expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient per visit.
#' @param param_var columm in \code{dataname} to select parameters 
#' @param param default parameter selected from \code{param_var}
#' @param param_choices vector of choices to choose the parameter from
#' @param value_var default columm in \code{dataname} to plot.
#' @param value_var_choices choice of which columns in \code{dataname} to plot
#' @param trt_group treatment variable.  The boxes and symbols are colored according
#'    to this parameter.  
#' @param trt_group_choices choices for \code{trt_group}.  If not specified then
#'    trt_group cannot be changed.
#' @param xaxis_var variable to categorize the x-axis
#' @param xaxis_var_choices choices for \code{xaxis_var}. if not specified then
#'    \code{xaxis_var} will be fixed and cannot be changed.
#' @param facet_var variable to facet the plots by.
#' @param facet_var_choices choices for \code{facet_var}. if not specified then
#'    \code{facet_var} will be fixed and cannot be changed.
#' @param armlabel header for the treatment symbols in the legend.  If not specified
#'    then the label attribute for \code{trt_group} will be used.  If there is 
#'    no label attribute for \code{trt_group}, then the name of the parameter (
#'    in title case) will be used.
#' @param color_manual vector of treatment colors. assigned values in app.R otherwise uses default colors.
#' @param shape_manual vector of LOQ shapes. assigned values in app.R otherwise uses default shapes.
#' @param plot_height  numeric vectors to define the plot height.
#' @param loq_flag_var variable for the LOQ.  Values are "Y" or "N"
#' @param hline y-axis value to position a horizontal line.  NULL = No line.
#' @param facet_ncol numeric value indicating number of facets per row.
#'     NULL = Use the default for ggplot2::facet_wrap.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param filter_vars Variables to be used for filtering the data.  The default 
#'    is BASE2 and BASE
#' @param filter_labs Labels for the radio buttons for the \code{filter_vars}.
#'    The defaults are "Screening" for BASE2 and "Baseline" for BASE.   
#' @param code_data_processing Not used
#' 
#' @inheritParams teal::standard_layout
#' 
#' @import DescTools
#' @import methods
#' @import utils
#' @import dplyr
#' @import goshawk
#' @import teal
#'
#' @author Balazs Toth
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @return an \code{\link[teal]{module}} object#'
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # Example using analysis dataset for example ASL or ADSL,
#' # ALB points to biomarker data stored in a typical LB structure. for example
#' # ALB or ADLB.
#' 
#' # Example using analysis dataset for example ASL or ADSL,
#' # ABM points to biomarker data stored in a custom file created to support
#' # goshawk. for example ADBIOM
#' library(dplyr) 
#'
#' # assign data frame note that this needs to be done once in the app.R file
#' # but should be available # here during testing
#' 
#' ASL <- ASL
#' ALB <- ALB
#'
#' x <- teal::init(
#'   data =  list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'       tm_g_boxplot(
#'         label = "Box Plot",
#'         dataname = "ALB",
#'         param_var = "PARAMCD",
#'         param = "IGA",
#'         param_choices = c("CRP", "IGA", "IGG", "IGM"),
#'         value_var = "AVAL",
#'         value_var_choices = c("AVAL", "BASE", "CHG"),
#'         rotate_xlab = FALSE,
#'         xaxis_var = "AVISITCD",
#'         trt_group = "ARM"
#'       )
#'   )
#' )
#' shinyApp(x$ui, x$server)
#'
#'
#'}

tm_g_boxplot <- function(label,
                         dataname,
                         param_var, # name of variable containing the biomarker names: PARAMCD
                         param, # biomarker selected
                         param_choices = param, # list of biomarkers of interest
                         value_var = "AVAL",
                         value_var_choices = c("AVAL", "CHG"),
                         plot_height = c(600, 200, 2000),
                         trt_group = "ARM",
                         color_manual = NULL,
                         shape_manual = NULL,
                         trt_group_choices = NULL,
                         facet_var = "ARM",
                         facet_var_choices = NULL,
                         xaxis_var = "AVISIT",
                         xaxis_var_choices = NULL,
                         loq_flag_var = NULL,
                         hline = NULL, 
                         facet_ncol = NULL, 
                         rotate_xlab = FALSE,
                         pre_output = NULL,
                         post_output = NULL,
                         armlabel = NULL,
                         filter_vars = c("BASE2", "BASE"),
                         filter_labs = c("Screening", "Baseline"),
                         code_data_processing = NULL) {
  
  args <- as.list(environment())

  # If there are no choices specified for treatment group/x axis/fact then set the 
  # appropriate choices variable to the treatment group to enable the display of the treatment 
  # group variable on the UI.
  if (is.null(args$trt_group_choices)) args$trt_group_choices = args$trt_group
  if (is.null(args$xaxis_var_choices)) args$xaxis_var_choices = args$xaxis_var
  if (is.null(args$facet_var_choices)) args$facet_var_choices = args$facet_var
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(dataname = dataname,
                       facet_var = facet_var,
                       facet_var_choices = facet_var_choices,
                       xaxis_var = xaxis_var,
                       xaxis_var_choices = xaxis_var_choices,
                       param_var = param_var,
                       value_var = value_var,
                       trt_group = trt_group,
                       trt_group_choices = trt_group_choices,
                       color_manual = color_manual,
                       shape_manual = shape_manual,
                       loq_flag_var = loq_flag_var,
                       armlabel = armlabel,
                       filter_vars = filter_vars,
                       filter_labs = filter_labs,
                       code_data_processing = code_data_processing
    ),
    ui = ui_g_boxplot,
    ui_args = args
  )
}

ui_g_boxplot <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  inpWidth <- NA
  
  standard_layout(
    output = div(tagList(uiOutput(ns("plot_ui")), uiOutput(ns("table_ui")))),
    encoding =  div(
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      
      optionalSelectInput(inputId = ns("param")
                         , label = " Select a Biomarker"
                         , choices = a$param_choices
                         , selected = a$param
                         , multiple = FALSE
                         , width = inpWidth
      ),
    
      optionalSelectInput(ns("value_var")
                          , label = "Select a Y-Axis Variable"
                          , choices = a$value_var_choices
                          , selected = a$value_var
                          , multiple = FALSE
                          , width = inpWidth
      ),
      
      optionalSelectInput(ns("xaxis_var")
                          , label = "Select an X-Axis Variable"
                          , choices = a$xaxis_var_choices
                          , selected = a$xaxis_var
                          , multiple = FALSE
                          , width = inpWidth
      ),
      
      optionalSelectInput(ns("facet_var")
                          , label = "Facet by"
                          , choices = a$facet_var_choices
                          , selected = a$facet_var
                          , multiple = FALSE
                          , width = inpWidth
      ),
      
      radioButtons(ns("y_filter_by"), 
                   "Data Constraint:",
                   inline = TRUE,
                   choiceNames = as.list(c("None", a$filter_labs)),
                   choiceValues = as.list(c("None", a$filter_vars))
                   ),
        div(id = ns("y_filter"), style="padding: 0px;",
            uiOutput(ns("y_select")),
            div(style="padding: 0px; margin: 0px",
              uiOutput(ns("ymin_value")
                           , style="display: inline-block; vertical-align:center;"),
              uiOutput(ns("yto"), style="display: inline-block; vertical-align:center;"),
              uiOutput(ns("ymax_value")
                           , style="display: inline-block; vertical-align:center;")
            )
      ),
      
      tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;"),
      
      numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, step = 1),
      
      checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
      
      numericInput(ns("hline"), "Add a Horizontal Line:", a$hline),
      
      uiOutput(ns("yaxis_scale")),

      optionalSliderInputValMinMax(ns("plot_height")
                                   , label = "Plot height"
                                   , a$plot_height
                                   , ticks = FALSE
                                   , step = 50
                                   , width = inpWidth),
      
      optionalSliderInputValMinMax(ns("font_size")
                                   , label = "Font Size"
                                   , a$font_size
                                   , value_min_max = c(12, 8, 20)
                                   , step = 1
                                   , ticks = FALSE
                                   , width = inpWidth
      ),
      
      optionalSliderInputValMinMax(ns("dot_size")
                                   , label = "Dot Size"
                                   , a$dot_size
                                   , value_min_max = c(2, 1, 12)
                                   , step = 1
                                   , ticks = FALSE
                                   , width = inpWidth
                                   ),
      
      optionalSliderInputValMinMax(ns("alpha")
                                   , label = "Dot Transparency"
                                   , value_min_max = c(0.8, 0.0, 1.0)
                                   , step = 0.1
                                   , ticks = FALSE
                                   , width = inpWidth
      )
      
    )
    # , forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    
  )
  
}

srv_g_boxplot <- function(input, output, session, datasets
                          , facet_var, facet_var_choices
                          , xaxis_var, xaxis_var_choices
                          , param_var, value_var
                          , trt_group, trt_group_choices
                          , color_manual, shape_manual
                          , loq_flag_var
                          , armlabel
                          , filter_vars, filter_labs
                          , dataname, code_data_processing) {
  
  # Get "nice" limits for Y axis. 
  get_axis_limits <- function(lo_, hi_, req.n = 2000) {
    hi <- signif(max(hi_, lo_), 6)
    lo <- signif(min(hi_, lo_), 6)
    
    if (req.n == 0) {
      return(list(min = lo, max = hi, step = 0, eqt = (hi == lo)))
    } else if (hi == lo) {
      return(list(min = lo-1, max = hi+1, step = 0.1, eqt = TRUE))
    } else {
      p <- pretty(c(lo, hi), n=10)
      d <- pretty(c(lo, hi), n=1.2*req.n, min.n = 0.5*req.n)
      return(list(min = head(p,1), max = tail(p,1), step = signif(d[2] - d[1], 8), eqt = FALSE))
    }
  }  
  
  # Extend is.infinite to include zero length objects.
  is_finite <- function(x){
    if(length(x) == 0) return(FALSE)
    return(is.finite(x))
  }
  
  chunks <- list(
    analysis = "# Not Calculated",
    table = "# Not calculated",
    boxsetup = " ",
    tablesetup =  " "
  )

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("boxplot"), height=plot_height)
  })
  
  # filter data by param and the xmin and xmax values from the filter slider.
  filter_ALB <- reactive({
    
    param <- input$param
    value_var <- input$value_var
    
    # Select all of the data for the parameter.
    alb <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter( eval(parse(text = param_var)) == param )
    
    # Only consider filtering on value if there is data to filter, the filter
    # inputs are loaded and that there are some non-missing values present.
    # Then check to see if the filter values have been altered from their default
    # values.  Only if all of these conditions have been met, filter on
    # the values.
    filt_var <- input$y_filter_by
    if (length(alb) & filt_var != "None" &
        is_finite(input$ymin) & is_finite(ylimits()$low)) {
      ymin_scale <- input$ymin
      ymax_scale <- input$ymax
      axlim <- get_axis_limits(min(cdata()[,filt_var], na.rm = T)
                               , max(cdata()[,filt_var], na.rm = T), req.n = 0 )
      
      if (!axlim$eqt & is_finite(ymin_scale) & is_finite(ymax_scale) & 
          ( ymin_scale != axlim$min | ymax_scale != axlim$max)){
        
        alb <- alb %>% 
          filter(ymin_scale <= eval(parse(text = filt_var)) & eval(parse(text = filt_var)) <= ymax_scale)
      }
    }
    return(alb)
      
  })
  
  # The current filtered data (by standard Teal filters)
  cdata <- reactive({
    req( input$param)
    ds <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE) %>%
      filter(eval(parse(text = param_var)) == input$param)
    return(ds)
  })
  
  # Y axis limits for the current data.  Protect against warning if there is 
  # no data selected.
  ylimits <- reactive({
    if (nrow(cdata()) == 0 ) 
      rng <- c( -Inf, Inf)
    else 
      rng <- range(cdata()[[input$value_var]], na.rm = TRUE)
    
    return(list(low = rng[1], high = rng[2]))
  })  
  
  # dynamic slider for y-axis - Use ylimits 
  observe({
    if (input$y_filter_by == "None") {
      output$y_select <- renderUI({
        HTML(
          paste0("<label>No Data Selection</label>")
        )
      })
      tagList({
        output$ymin_value <- renderUI({NULL})
        output$ymax_value <- renderUI({NULL})
      })
    } else {
      # Calculate nice default limits based on the min and max from the data
      ybase <- get_axis_limits(min(cdata()[,input$y_filter_by], na.rm = T)
                               , max(cdata()[,input$y_filter_by], na.rm = T)
                               , req.n = 0 )
      
      if (ybase$eqt) {
        output$y_select <- renderUI({
          HTML(
            paste0("<label>", input$y_filter_by, ": No selection possible (Min=Max))</label>")
          )
        })
      } else {
        output$y_select <- renderUI({NULL})
      }
      
      output$ymin_value <- renderUI({
        if (is_finite(ybase$min) & !ybase$eqt) {
          numericInput(session$ns("ymin")
                       , label = paste0("Min (", ybase$min, ")")
                       , value = ybase$min, min = ybase$min, max = ybase$max)
        } else {
          return(NULL)
        }
      })
      
      output$ymax_value <- renderUI({
        if (is_finite(ybase$max) & !ybase$eqt) {
          numericInput(session$ns("ymax")
                       , label = paste0("Max (", ybase$max, ")")
                       , value = ybase$max, min = ybase$min, max = ybase$max)
        } else {
          return(NULL)
        }
      })
    }
    
    output$yaxis_scale <- renderUI({
      yax <- get_axis_limits(ylimits()$low, ylimits()$high, req.n = 100)
      if (is_finite(yax$min) & is_finite(yax$max) ) {
        tagList({
          sliderInput(session$ns("yrange_scale")
                      , label = paste0("Y-Axis Range Zoom")
                      , min = yax$min
                      , max = yax$max
                      , step = yax$step
                      , value = c(yax$min, yax$max) 
          )
        })
      }
    })  
  })
  
  # If facet selection changes to be the same as x axis selection, set the
  # x axis selection to another value
  observeEvent(input$facet_var,{
    x <- input$xaxis_var
    f <- input$facet_var
    c <- xaxis_var_choices
    if (x == f & length(c) > 1) {
      s <- head(c[c != f],1)
      updateSelectInput(session,"xaxis_var", selected = s)
    }
  })

  observeEvent(input$xaxis_var,{
    x <- input$xaxis_var
    f <- input$facet_var
    c <- facet_var_choices
    if (x == f & length(c) > 1) {
      s <- head(c[c != x],1)
      updateSelectInput(session,"facet_var", selected = s)
    }
  })
  
  output$boxplot <- renderPlot({
    
    filter_var <- input$filter_var
    value_var <- input$value_var
    param <- input$param
    hline <- input$hline
    facet_ncol <- input$facet_ncol
    rotate_xlab = input$rotate_xlab
    dot_size <- input$dot_size
    font_size <- input$font_size
    alpha <- input$alpha
    xaxis_var <- input$xaxis_var
    facet_var <- input$facet_var
    
    # Get the filtered data - Filter by both the right an left filters. 
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ALB <- filter_ALB()
    
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    
    if (input$y_filter_by != "None") {    
      ymin <- input$ymin
      ymax <- input$ymax
      if (length(ymin) > 0 & !is.null(ymin)) {
        validate(need(ymax >= ymin,
                    paste("Minimum filter value greater than maximum filter value")))
      }
    }
    validate(need(!is.null(ALB) && is.data.frame(ALB), "No data left"))
    validate(need(nrow(ALB) > 0 , "No observations left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ALB),
                  paste("Variable", trt_group, " is not available in data", dataname)))
    validate(need(value_var %in% names(ALB),
                  paste("Variable", value_var, " is not available in data", dataname)))
    validate(need(xaxis_var %in% names(ALB),
                  paste("Variable", xaxis_var, " is not available in data", dataname)))
    validate(need(facet_var %in% names(ALB),
                  paste("Variable", facet_var, " is not available in data", dataname)))
    validate(need(filter_vars %in% names(ALB),
                  paste("Variable", filter_var, " is not available in data", dataname)))
    
    chunks$boxsetup <<- bquote({
      # Units to display, just take the first if there multiples.
      unit <- ALB %>% 
        filter(eval(parse(text = param_var)) == param) %>% 
        select("AVALU") %>% 
        unique() %>% 
        top_n(1,1) %>% 
        as.character()
    })
    eval(chunks$boxsetup)
    
    data_name <- paste0("ALB", "_FILTERED")
    assign(data_name, filter_ALB())

    chunks$analysis <<- call(
      "g_boxplot",
      data = bquote(.(as.name(data_name))),
        biomarker = param,
        value_var = value_var,
        hline = hline, 
        facet_ncol = facet_ncol,
        rotate_xlab = rotate_xlab,
        trt_group = trt_group,
        timepoint = "over time",
        unit = unit,
        ymin_scale = ymin_scale,
        ymax_scale = ymax_scale,
        loq_flag = loq_flag_var, 
        color_manual = color_manual,
        shape_manual = shape_manual,
        alpha = alpha,
        dot_size = dot_size,
        font_size = font_size,
        xaxis_var = xaxis_var,
        armlabel = armlabel,
        facet = facet_var
    )
    
    p <- try(eval(chunks$analysis))
    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create plot for box plot:\n\n", p)))
    p
  }
  )

   output$table_ui <- renderTable({
      ALB <- filter_ALB()

      param <- input$param
      xaxis_var <- input$value_var
      font_size <- input$font_size
      facet <- ifelse(facet_var_choices, input$facet_var, facet_var)

      data_name <- paste0("ALB", "_FILTERED")
      assign(data_name, ALB)
      
      chunks$table <<- call(
        "t_summarytable",
        data = bquote(.(as.name(data_name))),
        trt_group = trt_group,
        param_var = param_var,
        param = param,
        visit_var = facet_var,
        xaxis_var = xaxis_var, 
        font_size = font_size
      )
      t <- try(eval(chunks$table))
      if (is(t, "try-error")) validate(need(FALSE, paste0("could not create table for box plot:\n\n", p)))
      t
      
  })
  
   
   observeEvent(input$show_rcode, {
     
     header <- teal.tern:::get_rcode_header(
       title = "Box Plot",
          datanames = "ALB",
          datasets = datasets
       )

     library(teal.tern)
     str_rcode <- paste(c(
       "",
       header,
       "",
       teal.tern:::remove_enclosing_curly_braces(deparse(chunks$boxsetup)),
       "",
       deparse(chunks$analysis, width.cutoff = 50),
       "\n",
       deparse(chunks$table, width.cutoff = 50)
       
     ), collapse = "\n")
     
     showModal(modalDialog(
       title = "R Code for the Current box Plot",
       tags$pre(tags$code(class="R", str_rcode)),
       easyClose = TRUE
     ))
   })
   
}
