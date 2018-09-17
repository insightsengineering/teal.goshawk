#' Teal Module: density distribution plot
#'
#' This shiny module displays a density distribution plot
#'
#' @param label menu item label of the module in the teal app 
#' @param dataname analysis data set name. needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data are expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient per visit.
#'
#' @inheritParams teal::standard_layout
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' @import goshawk
#' @import teal
#'
#' @author Balazs Toth
#' @author Nick Paszty
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
#'
#'ASL0 <- read_bce(ASL_path)
#'ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')
#'
#'ALB0 <- read_bce(ALB_path)
#'
#'# post process the data to subset records per specification
#'ALB_SUBSET <- subset(ALB0,
#'               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
#'               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
#'                'LBSTRESC', 'LBSTRESN'))
#'
#' # calculate the minimum AVAL for each PARAMCD
#' PARAM_MINS <- ALB_SUBSET %>%
#' select(USUBJID, PARAMCD, AVAL) %>%
#'   filter(PARAMCD %in% param_choices) %>%
#'   group_by(PARAMCD) %>%
#'   summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))
#'   
#'# post process the data to create several new variables and adjust existing record specific valules per specification
#'# - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
#'# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
#'ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'                mutate(AVISITCD = ifelse(AVISITCD == "BB", "BL", AVISITCD)) %>%
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BL", 0, substr(AVISITCD,start=2, stop=4))) %>%
#'                mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
#'                mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
#'                mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG))
#'                # may need to add similar code for BASE2 related variables
#'
#'   
#' # merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
#' ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
#'        mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
#'        mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
#'        #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))
#'
#'# for proper chronological ordering of visits in visualizations
#'ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
#'ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#'
#'# to test loq_flag_var
#'ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'     tm_g_density_distribution_plot(
#'        label = "Density Distribution Plot",
#'        dataname = "ALB",
#'        param_var = "PARAMCD",
#'        param_choices = param_choices,
#'        param = "CRP",
#'        xaxis_var = "AVAL",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
#'        trt_group = "ARM",
#'        trt_group_choices = c("ARM", "ARMCD"),
#'        loq_flag_var = 'LOQFL',
#'        plot_width = c(800, 200, 2000),
#'        plot_height = c(500, 200, 2000),
#'        font_size = c(12, 8, 20),
#'        line_size = c(1, 1, 12)
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'}

tm_g_density_distribution_plot <- function(label, # label of module
                                           dataname, # analysis data set
                                           param_var, # name of variable containing the biomarker names: PARAMCD
                                           param, # biomarker selected
                                           param_choices = param, # list of biomarkers of interest
                                           xaxis_var, # name of variable containing values displayed on the x-axis
                                           xaxis_var_choices = xaxis_var, # list of baseline variables
                                           trt_group,
                                           trt_group_choices,
                                           loq_flag_var = 'LOQFL',
                                           plot_width,
                                           plot_height,
                                           font_size,
                                           line_size,
                                           hline = NULL,
                                           rotate_xlab = FALSE,
                                           man_color = NULL,
                                           pre_output = NULL,
                                           post_output = helpText("Variable Name Legend: BASE2 = Screening Data, BASE2L2 = Log2(BASE2);
                                                    CHG2 = Change from Screening, PCHG2 =  %Change from Screening;
                                                    BASE = BASELINE Data, BASEL2 = Log2(BASE);
                                                    CHG = Change from Baseline, PCHG =  %Change from Baseline;
                                                    AVAL = Following Visits, AVALL2 = Log2(AVAL)"),
                                           code_data_processing = NULL) {

  args <- as.list(environment())
  
  # for debugging
  #print(paste0('Where assigned, ARGS is now: ', args))

  module(
    label = label,
    filters = dataname,
    server = srv_g_density_distribution_plot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       param = param,
                       xaxis_var = xaxis_var,
                       trt_group = trt_group,
                       code_data_processing = code_data_processing
                       ),
    ui = ui_g_density_distribution_plot,
    ui_args = args
  )
  
}

ui_g_density_distribution_plot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)
  
  # for debugging
  #print(paste0('Where assigned, a is now: ', a))
  
  standard_layout(
    output = div(tagList(uiOutput(ns("plot_ui")), uiOutput(ns("table_ui")))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("param"), "Select a Biomarker", a$param_choices, a$param, multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      optionalSliderInputValMinMax(ns("plot_width"), "Plot Width", a$plot_width, ticks = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      uiOutput(ns("xaxis_scale")),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("line_size"), "Line Size", a$line_size, ticks = FALSE)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_density_distribution_plot <- function(input, output, session, datasets, dataname, param_var, param, xaxis_var, trt_group, code_data_processing) {

  ns <- session$ns # must add for the dynamic ui.range_scale field
  
  # dynamic plot width
  output$plot_ui <- renderUI({
    plot_width <- input$plot_width
    validate(need(plot_width, "need valid plot width"))
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("density_distribution_plot"), width = plot_width, height = plot_height)
    })
  
  # dynamic slider for x-axis
  output$xaxis_scale <- renderUI({
    ALB <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # must add for the dynamic ui.range_scale field
    param <- input$param # must add for the dynamic ui.range_scale field
    scale_data <- ALB %>%
      filter(eval(parse(text = param_var)) == param)
    
      # identify min and max values of BM range ignoring NA values
      xmin_scale <- min(scale_data[[input$xaxis_var]], na.rm = TRUE)
      xmax_scale <- max(scale_data[[input$xaxis_var]], na.rm = TRUE)

      tagList({
        sliderInput(ns("xrange_scale"), label="X-Axis Range Scale", xmin_scale, xmax_scale, value = c(xmin_scale, xmax_scale))
      })

  })

  output$density_distribution_plot <- renderPlot({
    
    # chunks <- list(
    #   analysis = "# Not Calculated"
    # )
    
    ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param
    xaxis_var <- input$xaxis_var
    font_size <- input$font_size
    line_size <- input$line_size
    hline <- as.numeric(input$hline)
    xmin_scale <- input$xrange_scale[1]
    xmax_scale <- input$xrange_scale[2]
    rotate_xlab <- input$rotate_xlab

    validate(need(!is.null(ALB) && is.data.frame(ALB), "no data left"))
    validate(need(nrow(ALB) > 0 , "no observations left")) # this seems counter intuitive
    validate(need(param_var %in% names(ALB),
                  paste("Biomarker parameter variable", param_var, " is not available in data", dataname)))
    validate(need(param %in% unique(ALB[[param_var]]),
                  paste("Biomarker", param, " is not available in data", dataname)))
    validate(need(trt_group %in% names(ALB),
                  paste("variable", trt_group, " is not available in data", dataname)))
    validate(need(xaxis_var %in% names(ALB),
                  paste("variable", xaxis_var, " is not available in data", dataname)))

        p <- goshawk:::g_density_distribution_plot(
      data = ALB,
      param_var = param_var,
      param = param,
      xaxis_var = xaxis_var,
      trt_group = trt_group,
      color_manual = NULL,
      xmin_scale = xmin_scale,
      xmax_scale = xmax_scale,
      font_size = font_size,
      line_size = line_size,
      rotate_xlab = rotate_xlab,
      hline = hline
    )

    p
    
  })

 output$table_ui <- renderTable({

      ALB <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
      param <- input$param
      xaxis_var <- input$xaxis_var
      font_size <- input$font_size

  t <- goshawk:::t_summarytable(
  data = ALB,
  trt_group = trt_group,
  param_var = param_var,
  param = param,
  xaxis_var = xaxis_var,
  font_size = font_size
  )

    t

  })

}
