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
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2)))
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
#'        value_var_choices = c("AVAL", "CHG", "PCHG"),
#'        baseline_var = "BASE",
#'        baseline_var_choices = c("BASE", "BASEL2", "SCRN", "SCRNL2"),
#'        trt_group = "ARM",
#'        trt_group_choices = c("ARM", "ARMCD"),
#'        plot_height = c(600, 200, 2000),
#'        m_facet = FALSE
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_scatterplot <- function(label, # label of module
                             dataname, # analysis data set
                             #filter_var = NULL, # not sure yet
                             #filter_var_choices = NULL, # not sure yet
                             param_var, # name of variable containing the biomarker names: PARAMCD
                             param, # biomarker selected
                             param_choices = param, # list of biomarkers of interest
                             #visit_var, # name of variable containing the visit values: AVISIT
                             #visits, # visits selected
                             #visit_var_choices = visits, # list of visits to plot
                             baseline_var, # name of variable containing the baseline values: BASE, SCRN, ...
                             baseline_var_choices = baseline_var, # list of baseline variables
                             #analysis_var, # name of variable containing values to analyze: AVAL, CHG, PCHG, ...
                             value_var, # analysis variable selected
                             value_var_choices = value_var, # list of analysis variables to plot
                             trt_group,
                             trt_group_choices,
                             plot_height,
                             m_facet = FALSE,
                             alpha = c(1, 0, 1),
                             size = c(4, 1, 12),
                             hline = NULL,
                             rotate_xlab = FALSE,
                             man_color = NULL,
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
      helpText("Default Analysis Variable:", tags$code(a$value_var)),
      optionalSelectInput(ns("value_var"), "Select an Analysis Variable", a$value_var_choices, a$value_var, multiple = FALSE),
      helpText("Default Baseline Variable:", tags$code(a$baseline_var)),
      optionalSelectInput(ns("baseline_var"), "Select a Baseline Variable", a$baseline_var_choices, a$baseline_var, multiple = FALSE),
      helpText("Default Biomarker:", tags$code(a$param)),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      #optionalSelectInput(ns("visit_var"), "Visit Variable", a$visit_var_choices, a$visit_var, multiple = TRUE),
      #optionalSelectInput(ns("bl_visit_var"), "Baseline Visit Variable", a$bl_visit_var_choices, a$bl_visit_var, multiple = FALSE),
      #optionalSelectInput(ns("filter_var"), "Preset Data Filters", tags$br, a$filter_var_choices, a$filter_var, multiple = TRUE),
      optionalSelectInput(ns("trt_group"), "Arm Variable", a$trt_group_choices, a$trt_group, multiple = FALSE),

      # add additional input statements here
      if (all(c(
        length(a$plot_height) == 1,
        length(a$size) == 1,
        length(a$alpha) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;")
      },
      checkboxInput(ns("m_facet"), "Treatment Facetting", a$m_facet),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "opacity", a$alpha, ticks = FALSE),
      optionalSliderInputValMinMax(ns("size"), "point size", a$size, ticks = FALSE)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_scatterplot <- function(input, output, session, datasets, dataname, param_var, value_var, baseline_var, trt_group, code_data_processing) {

  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("scatterplot"), height=plot_height)
  })

  output$scatterplot <- renderPlot({
    
    # chunks <- list(
    #   analysis = "# Not Calculated"
    # )
    
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param
    value_var <- input$value_var
    baseline_var <- input$baseline_var
    alpha <- input$alpha
    size <- input$size
    hline <- as.numeric(input$hline)
    m_facet <- input$m_facet
    rotate_xlab <- input$rotate_xlab
    
    validate(need(alpha, "need alpha"))    
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
      rotate_xlab = rotate_xlab,
      hline = hline
    )

    p

  })
    
}
 