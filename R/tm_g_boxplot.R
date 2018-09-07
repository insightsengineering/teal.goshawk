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
#' @param facet_var variable on which to facet the plot. A boxplot will
#'    be produced on the screen for each value of \code{facet_var}. 
#'    "None" is the default, which will display one boxplot.
#' @param facet_choices choices for faceting.  Default is "None" for no facets.
#' @param arm_var treatment variable
#' @param arm_var_choices choices for \code{arm_var}
#'
#' @inheritParams teal::standard_layout
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
#'\dontrun{
#'
#' # Example using analysis dataset for example ASL or ADSL,
#' # ABM points to biomarker data stored in a custom file created to support goshawk. for example ADBIOM
#' library(dplyr)
#'
#' # assign data frame note that this needs to be done once in the app.R file but should be available
#' # here during testing
#' ASL <- ASL
#' ABM <- ALB
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ABM = ABM),
#'   modules = root_modules(
#'         tm_g_boxplot(
#'           label = "Box Plot",
#'           dataname = "ALB",
#'           param_var = "PARAMCD",
#'           param = "IGA",
#'           param_choices = c("IGA","IGG","IGM"),
#'           value_var = "AVAL",
#'           value_var_choices = c("AVAL", "CHG"),
#'           facet_var = "None",
#'           facet_choices = c("None", "ARM", "AVISIT"),
#'           arm_var = "ARM",
#'           arm_var_choices = c("ARM", "ARMCD")
#'         )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
#' 
tm_g_boxplot <- function(label,
                         dataname,
                         filter_var = NULL,
                         filter_var_choices = NULL,
                         param_var, # name of variable containing the biomarker names: PARAMCD
                         param, # biomarker selected
                         param_choices = param, # list of biomarkers of interest
                         value_var = "AVAL",
                         value_var_choices = c("AVAL", "CHG"),
                         plot_height = c(600, 200, 2000),
                         arm_var,
                         arm_var_choices,
                         facet_var = NULL,
                         facet_choices = NULL,
                         pre_output = NULL,
                         post_output = NULL,
                         code_data_processing = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       value_var = value_var,
                       arm_var = arm_var,
                       code_data_processing = code_data_processing
    ),
    ui = ui_g_boxplot,
    ui_args = args
  )
}

ui_g_boxplot <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("filter_var")
                          , label = "Preset Data Filters"
                          , choices = a$filter_var_choices
                          , selected = a$filter_var
                          , multiple = TRUE
      ),
      
      optionalSelectInput(ns("arm_var")
                          , label = "Arm Variable"
                          , choices = a$arm_var_choices
                          , selected = a$arm_var
                          , multiple = FALSE
      ),
      
      optionalSelectInput(inputId = ns("param")
                          , label = "Select a Biomarker"
                          , choices = a$param_choices
                          , selected = a$param
                          , multiple = FALSE
      ),
      
      optionalSelectInput(ns("value_var")
                          , label = "Select an Analysis Variable"
                          , choices = a$value_var_choices
                          , selected = a$value_var
                          , multiple = FALSE
      ),
      
      optionalSelectInput(ns("facet_var")
                          , label = "Faceting Variable"
                          , choices = a$facet_choices
                          , selected = a$facet_var
                          , multiple = FALSE),
      
      uiOutput(ns("yaxis_scale")),
      
      optionalSliderInputValMinMax(ns("plot_height")
                                   , label = "plot height"
                                   , a$plot_height
                                   , ticks = FALSE
                                   , step = 50)
    )
  )
  
}

srv_g_boxplot <- function(input, output, session, datasets, param_var, value_var, arm_var, dataname, code_data_processing) {
  
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
  
  # Y axis limits for the current data.
  ylimits <- reactive({
    rng <- range(cdata()[[input$value_var]], na.rm = TRUE)
    return(list(low =rng[1], high = rng[2]))
  })  
  
  # dynamic slider for y-axis - Use ylimits 
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
    
    tagList({
      sliderInput(session$ns("yrange_scale")
                  , label=paste("Y-Axis Range Scale for",param)
                  , ymin_scale, ymax_scale
                  , value = c(ymin_scale, ymax_scale))
    })
  })  
  
  output$boxplot <- renderPlot({
    
    filter_var <- input$filter_var
    arm_var <- input$arm_var
    value_var <- input$value_var
    param <- input$param
    facet_var <- input$facet_var
    
    # Get the filtered data - Filter by both the right an left filters. 
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ALB <- cdata()    
    
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    
    # Units to display, just take the first if there multiples.
    unit <- ALB %>% 
      filter(eval(parse(text = param_var)) == param) %>% 
      select(AVALU) %>% 
      unique() %>% 
      top_n(1,1) %>% 
      as.character()
    
    # asl_vars <- c("USUBJID", "STUDYID")
    # if (arm_var %in% names(ASL_FILTERED)) {
    #   asl_vars <- unique(asl_vars, arm_var)
    # } 
    # abm_vars <- unique(c("USUBJID", "STUDYID", arm_var, 'PARAMCD', value_var))
    
    validate(need(!is.null(ALB) && is.data.frame(ALB), "no data left"))
    validate(need(nrow(ALB) > 0 , "no observations left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(arm_var %in% names(ALB),
                  paste("variable", arm_var, " is not available in data", dataname)))
    validate(need(value_var %in% names(ALB),
                  paste("variable", value_var, " is not available in data", dataname)))
    
    p <- g_boxplot(
      data = ALB,
      biomarker = param,
      value_var = value_var,
      arm = arm_var,
      timepoint = "over time", 
      unit = unit,
      ymin_scale = ymin_scale,
      ymax_scale = ymax_scale, 
      color_manual = c('ARM 1' = "#1F78B4", 'ARM 2' = "#33A02C", 'ARM 3' = "#601010"),
      shape_manual = c('N' = 1, 'Y' = 2, 'NA' = NULL),
      facet = facet_var
    )
    
    p
  }
  )
}
