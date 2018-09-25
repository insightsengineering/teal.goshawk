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
#' @param trt_group treatment variable
#' @param trt_group_choices choices for \code{trt_group}
#' @param visit_var default columm in \code{dataname} to use for visit.
#' @param visit_var_choices choice of which columns in \code{dataname} to use
#'   for visit.  If there is just one value in visit_var_choices, then this will
#'   be used and no selection of visit_var will be available.
#' @param plot_height  numeric vectors to define the plot height.
#' @param facet boolen - if TRUE facet the display
#' @param facet_choices boolean - if TRUE display a check box for facetting
#' @param loq_flag_var variable for the LOQ.  Values are "Y" or "N"
#' @param code_data_processing TODO
#' 
#' @inheritParams teal::standard_layout
#' 
#' @import DescTools
#' @import methods
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
#' #' x <- teal::init(
#'   data =  list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'       tm_g_boxplot(
#'         label = "Box Plot",
#'         dataname = "ALB",
#'         param_var = "PARAMCD",
#'         param = "IGA",
#'         param_choices = c("IGA", "IGG", "IGM"),
#'         value_var = "AVAL",
#'         value_var_choices = c("AVAL", "BASE", "CHG"),
#'         visit_var = "AVISIT",
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
                         trt_group_choices = NULL,
                         visit_var = "AVISIT",
                         visit_var_choices = NULL,
                         facet_choices = FALSE,
                         facet = TRUE,
                         loq_flag_var = NULL,
                         pre_output = NULL,
                         post_output = NULL,
                         code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  # If there are no choices specified for visit_var then set to visit_var to
  # enable the default display of the selected visit variable in the UI
  if (is.null(args$visit_var_choices)) args$visit_var_choices = args$visit_var
  
  # If there are no choices specified for treatment group then set the choices 
  # variable to the treatment group to enable the display of the treatment 
  # group variable on the UI.
  if (is.null(args$trt_group_choices)) args$trt_group_choices = args$trt_group
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(dataname = dataname,
                       facet = facet,
                       facet_choices = facet_choices,
                       visit_var = visit_var,
                       visit_var_choices = visit_var_choices,
                       param_var = param_var,
                       value_var = value_var,
                       trt_group = trt_group,
                       trt_group_choices = trt_group_choices,
                       loq_flag_var = loq_flag_var,
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
      tags$label("Encodings", class="text-primary"),
      
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("filter_var")
                          , label = "Preset Data Filters"
                          , choices = a$filter_var_choices
                          , selected = a$filter_var
                          , multiple = TRUE
                          , width = inpWidth
                          ),
      
      optionalSelectInput(ns("trt_group")
                          , label = "Treatment Group"
                          , choices = a$trt_group_choices
                          , selected = a$trt_group
                          , multiple = FALSE
                          , width = inpWidth
      ),
      
      optionalSelectInput(ns("visit_var")
                          , label = "Visit Variable"
                          , choices = a$visit_var_choices
                          , selected = a$visit_var
                          , multiple = FALSE
                          , width = inpWidth
      ),
      
      optionalSelectInput(inputId = ns("param")
                         , label = "Biomarker"
                         , choices = a$param_choices
                         , selected = a$param
                         , multiple = FALSE
                         , width = inpWidth
      ),
    
      optionalSelectInput(ns("value_var")
                          , label = "Analysis Variable"
                          , choices = a$value_var_choices
                          , selected = a$value_var
                          , multiple = FALSE
                          , width = inpWidth
      ),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      
      # Only display the facet by visit checkbox if faceting choices given.
      if(a$facet_choices) {
        checkboxInput(ns("facet"), "Visit Facetting", a$facet)
      },

      optionalSliderInputValMinMax(ns("plot_height")
                                   , label = "Plot height"
                                   , a$plot_height
                                   , ticks = FALSE
                                   , step = 50
                                   , width = inpWidth),
      
      uiOutput(ns("yaxis_scale")),

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
      
    ), 
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    
  )
  
}

srv_g_boxplot <- function(input, output, session, datasets
                          , facet, facet_choices
                          , visit_var, visit_var_choices
                          , param_var, value_var, trt_group, trt_group_choices
                          , loq_flag_var
                          , dataname, code_data_processing) {
  
  chunks <<- list(
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
  
  # The current filtered data.
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
    output$yaxis_scale <- renderUI({
      param <- input$param # must add for the dynamic ui.range_scale field
  
      # Calculate nice default limits based on the min and max from the data
      lo <- ylimits()$low
      hi <- ylimits()$high
      exp <- floor(log10(hi - lo))
      f <- (hi - lo)/(10^exp)
      nndiff <- 10^(exp-1) *
        case_when(
          f <= 1.0 ~ 1,
          f <= 2.0 ~ 2,
          f <= 5.0 ~ 5, 
          TRUE ~ 10
        )
      
      ymin_scale <- RoundTo(lo, multiple = nndiff, FUN = floor)
      ymax_scale <- RoundTo(hi, multiple = nndiff, FUN = ceiling)

      if (is.finite(ymin_scale)) {
        tagList({
          sliderInput(session$ns("yrange_scale")
                      , label=paste("Y-Axis Range for",param)
                      , ymin_scale, ymax_scale
                      , value = c(ymin_scale, ymax_scale)
                      )
        })
      }
    })  
  })
  
  output$boxplot <- renderPlot({
    
    filter_var <- input$filter_var
    trt_group <- ifelse(!is.null(trt_group_choices) , input$trt_group, trt_group)
    value_var <- input$value_var
    visit_var <- input$visit_var
    param <- input$param
    if (facet_choices) facet <- input$facet
    dot_size <- input$dot_size
    font_size <- input$font_size
    alpha <- input$alpha
    
    # Get the filtered data - Filter by both the right an left filters. 
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ALB <- datasets$get_data("ALB", reactive = TRUE, filtered = TRUE)   
    # attr(ALB, "source") <- "Local filtered" 
    
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    
    validate(need(!is.null(ALB) && is.data.frame(ALB), "no data left"))
    validate(need(nrow(ALB) > 0 , "no observations left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ALB),
                  paste("variable", trt_group, " is not available in data", dataname)))
    validate(need(value_var %in% names(ALB),
                  paste("variable", value_var, " is not available in data", dataname)))
    
    chunks$boxsetup <<- bquote({
      # If no facet variable has been specified and we're to facet, then set to "None".
      facet_var <- ifelse(facet == T, visit_var, "None")
  
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
    assign(data_name, ALB)
    
    chunks$analysis <<- call(
      "g_boxplot",
      data = bquote(.(as.name(data_name))),
        biomarker = param,
        value_var = value_var,
        trt_group = trt_group,
        timepoint = "over time",
        unit = unit,
        ymin_scale = ymin_scale,
        ymax_scale = ymax_scale,
        loq_flag = loq_flag_var, 
        color_manual = NULL,
        shape_manual = c('N' = 1, 'Y' = 2, 'NA' = NULL),
        alpha = alpha,
        dot_size = dot_size,
        font_size = font_size,
        facet = facet_var
    )
    
    p <- try(eval(chunks$analysis))
    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create plot for box plot:\n\n", p)))
    p
  }
  )

   output$table_ui <- renderTable({
      ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

      param <- input$param
      xaxis_var <- input$value_var
      font_size <- input$font_size
      facet <- ifelse(facet_choices, input$facet, facet)
      visit_var <- input$visit_var
      
      # If the data isn't faceted by visit and there is more than one visit
      # present in the data, combine the visit data for the summary.
      chunks$tablesetup <<- bquote({
        if (!facet) {
          numVis <- length(unique(ALB[,visit_var]))
          if (numVis > 1) {
            ALB[,visit_var] <- "All"
          }
        }
      }) 

      data_name <- paste0("ALB", "_FILTERED")
      assign(data_name, ALB)
      
      chunks$table <<- call(
        "t_summarytable",
        data = bquote(.(as.name(data_name))),
        trt_group = trt_group,
        param_var = param_var,
        param = param,
        visit_var = visit_var,
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
       teal.tern:::remove_enclosing_curly_braces(deparse(chunks$tablesetup)),
       "",
       deparse(chunks$table, width.cutoff = 50)
       
     ), collapse = "\n")
     
     showModal(modalDialog(
       title = "R Code for the Current box Plot",
       tags$pre(tags$code(class="R", str_rcode)),
       easyClose = TRUE
     ))
   })
   
}
