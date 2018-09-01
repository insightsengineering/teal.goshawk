#' Teal Module: scatter plot
#'
#' This shiny module displays a scatter plot
#'
#' @param label menu item label of the module in the teal app 
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data are expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient per visit.
#'
#' @inheritParams teal::standard_layout
#'
#' @author Balazs Toth
#' @author first last
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
#' # ALB points to biomarker data stored in a typical LB structure. for example ALB or ADLB.
#'
#'# for development team testing
#'ASL_path <- "~/btk/lupus/dataadam/asl.sas7bdat"
#'ALB_path <- "~/btk/lupus/dataadam/alb3arm.sas7bdat"
#'
#'# list of biomarkers of interest. see ALB2 assignment below
#'param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")
#'value_var_choices <- c("AVAL","CHG","PCHG")
#'
#'ASL <- read_bce(ASL_path)
#'ALB0 <- read_bce(ALB_path)
#'
#'# post process the data to subset records per specification and to create new variables
#'ALB1 <- subset(ALB0,
#'               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
#'               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG'))
#'
#'# create a visit code - baseline record code is "BB" week records coded to "W NN"
#'ALB <- ALB1 %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4)))
#'                
#'ALB$AVISITCDN <- as.numeric(ALB$AVISITCDN) # coerce character into numeric
#'
#'# for proper chronological ordering of visits in visualizations
#'ALB <- ALB %>%
#'       mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#'
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'        label = "Scatter Plot",
#'        dataname = "ALB",
#'        param_var = "PARAMCD",
#'        param_choices = param_choices,
#'        param = "CRP",
#'        value_var = "AVAL",
#'        value_var_choices = c("AVAL", "BASE", "CHG", "PCHG"),
#'        baseline_var = "BASE",
#'        baseline_var_choices = c("AVAL", "BASE", "CHG", "PCHG"),
#'        trt_group = "ARM",
#'        trt_group_choices = c("ARM", "ARMCD"),
#'        plot_width = c(800, 200, 2000),
#'        plot_height = c(800, 200, 2000),
#'        m_facet = FALSE,
#'        reg_line = FALSE,
#'        dot_size = c(1, 1, 12)
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_scatterplot <- function(label, # label of module
                             dataname, # analysis data set
                             param_var, # name of variable containing the biomarker names: PARAMCD
                             param, # biomarker selected
                             param_choices = param, # list of biomarkers of interest
                             baseline_var, # name of variable containing values displayed on the x-axis
                             baseline_var_choices = baseline_var, # list of baseline variables
                             value_var, # name of variable containing values displayed on the y-axis
                             value_var_choices = value_var, # list of analysis variables to plot
                             trt_group,
                             trt_group_choices,
                             plot_width,
                             plot_height,
                             m_facet = FALSE,
                             reg_line = FALSE,
                             dot_size, # dot size
                             hline = NULL,
                             rotate_xlab = FALSE,
                             man_color = NULL,
                             pre_output = NULL,
                             post_output = helpText("Scatter Plot Specific Gutter Comment"),
                             code_data_processing = NULL) {

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_scatterplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       param = param,
                       value_var = value_var,
                       baseline_var = baseline_var,
                       trt_group = trt_group,
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
    output = uiOutput(ns("plot_ui")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Dataset:", tags$code(a$dataname)),
      #helpText("Default Biomarker:", tags$code(a$param)),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      #helpText("Default X-Axis Variable:", tags$code(a$baseline_var)),
      optionalSelectInput(ns("baseline_var"), "Select an X-Axis Variable", a$baseline_var_choices, a$baseline_var, multiple = FALSE),
      #helpText("Default Y-Axis Variable:", tags$code(a$value_var)),
      optionalSelectInput(ns("value_var"), "Select a Y-Axis Variable", a$value_var_choices, a$value_var, multiple = FALSE),
      #optionalSelectInput(ns("trt_group"), "Arm Variable", a$trt_group_choices, a$trt_group, multiple = FALSE),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      checkboxInput(ns("m_facet"), "Treatment Facetting", a$m_facet),
      checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      optionalSliderInputValMinMax(ns("plot_width"), "Plot Width", a$plot_width, ticks = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      uiOutput(ns("xaxis_scale")),
      uiOutput(ns("yaxis_scale")),
      optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_scatterplot <- function(input, output, session, datasets, dataname, param_var, param, value_var, baseline_var, trt_group, code_data_processing) {

  ns <- session$ns # must add for the dynamic ui.range_scale field
  
  # dynamic plot width
  output$plot_ui <- renderUI({
    plot_width <- input$plot_width
    validate(need(plot_width, "need valid plot width"))
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("scatterplot"), width = plot_width, height = plot_height)
    })
  
  # dynamic slider for x-axis
  output$xaxis_scale <- renderUI({
    ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # must add for the dynamic ui.range_scale field
    param <- input$param # must add for the dynamic ui.range_scale field
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == param)
    
      # identify min and max values of BM range ignoring NA values
      xmin_scale <- min(scale_data[[input$baseline_var]], na.rm = TRUE)
      xmax_scale <- max(scale_data[[input$baseline_var]], na.rm = TRUE)

      tagList({
        sliderInput(ns("xrange_scale"), label="X-Axis Range Scale", xmin_scale, xmax_scale, value = c(xmin_scale, xmax_scale))
      })

  })

  # dynamic slider for y-axis
  output$yaxis_scale <- renderUI({
    ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # must add for the dynamic ui.range_scale field
    param <- input$param # must add for the dynamic ui.range_scale field
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == param)

    # identify min and max values of BM range ignoring NA values
    ymin_scale <- min(scale_data[[input$value_var]], na.rm = TRUE)
    ymax_scale <- max(scale_data[[input$value_var]], na.rm = TRUE)

    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Scale", ymin_scale, ymax_scale, value = c(ymin_scale, ymax_scale))
    })

  })
  
  output$scatterplot <- renderPlot({
    
    # chunks <- list(
    #   analysis = "# Not Calculated"
    # )
    
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param
    value_var <- input$value_var
    baseline_var <- input$baseline_var
    dot_size <- input$dot_size
    hline <- as.numeric(input$hline)
    m_facet <- input$m_facet
    reg_line <- input$reg_line
    xmin_scale <- input$xrange_scale[1]
    xmax_scale <- input$xrange_scale[2]
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    rotate_xlab <- input$rotate_xlab

    validate(need(!is.null(ALB) && is.data.frame(ALB), "no data left"))
    validate(need(nrow(ALB) > 0 , "no observations left"))
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ALB),
                  paste("variable", trt_group, " is not available in data", dataname)))
    validate(need(baseline_var %in% names(ALB),
                  paste("variable", baseline_var, " is not available in data", dataname)))
    
    p <- goshawk:::g_scatterplot(
      data = ALB,
      biomarker_var = param_var,
      biomarker = param,
      value_var = value_var,
      value_var_bl = baseline_var,
      trt_group = trt_group,
      color_manual = NULL,
      f_facet = m_facet,
      reg_line = reg_line,
      xmin_scale = xmin_scale,
      xmax_scale = xmax_scale,
      ymin_scale = ymin_scale,
      ymax_scale = ymax_scale,
      dot_size = dot_size,
      rotate_xlab = rotate_xlab,
      hline = hline
    )

    p

  })
    
}
 