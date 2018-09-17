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
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' @import goshawk
#' @import teal
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
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4))) %>%
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
#'# to test loq_flag
#'ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
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
#'        xaxis_var = "BASE",
#'        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
#'        yaxis_var = "AVAL",
#'        yaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
#'        trt_group = "ARM",
#'        trt_group_choices = c("ARM", "ARMCD"),
#'        plot_width = c(800, 200, 2000),
#'        plot_height = c(800, 200, 2000),
#'        facet = FALSE,
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

tm_g_scatterplot <- function(label, # label of module
                             dataname, # analysis data set
                             param_var, # name of variable containing the biomarker names: PARAMCD
                             param, # biomarker selected
                             param_choices = param, # list of biomarkers of interest
                             xaxis_var, # name of variable containing values displayed on the x-axis
                             xaxis_var_choices = xaxis_var, # list of baseline variables
                             yaxis_var, # name of variable containing values displayed on the y-axis
                             yaxis_var_choices = yaxis_var, # list of analysis variables to plot
                             trt_group,
                             trt_group_choices,
                             plot_width,
                             plot_height,
                             facet = FALSE,
                             reg_line = FALSE,
                             font_size,
                             dot_size,
                             reg_text_size,
                             hline = NULL,
                             rotate_xlab = FALSE,
                             man_color = NULL,
                             pre_output = NULL,
                             post_output = helpText("BASE2 = Screening Data, BASE2L2 = Log2(BASE2);
                                                    CHG2 = Change from Screening, PCHG2 =  %Change from Screening;
                                                    BASE = BASELINE Data, BASEL2 = Log2(BASE);
                                                    CHG = Change from Baseline, PCHG =  %Change from Baseline;
                                                    AVAL = Following Visits, AVALL2 = Log2(AVAL)"),
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
      #helpText("Default X-Axis Variable:", tags$code(a$xaxis_var)),
      optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable", a$xaxis_var_choices, a$xaxis_var, multiple = FALSE),
      #helpText("Default Y-Axis Variable:", tags$code(a$yaxis_var)),
      optionalSelectInput(ns("yaxis_var"), "Select a Y-Axis Variable", a$yaxis_var_choices, a$yaxis_var, multiple = FALSE),
      #optionalSelectInput(ns("trt_group"), "Arm Variable", a$trt_group_choices, a$trt_group, multiple = FALSE),

      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      checkboxInput(ns("facet"), "Treatment Facetting", a$facet),
      checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      optionalSliderInputValMinMax(ns("plot_width"), "Plot Width", a$plot_width, ticks = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      uiOutput(ns("xaxis_scale")),
      uiOutput(ns("yaxis_scale")),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("reg_text_size"), "Regression Annotations Size", a$reg_text_size, ticks = FALSE)
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")#,
      # downloadButton(ns("export_plot"), "Export Image", width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g_scatterplot <- function(input, output, session, datasets, dataname, param_var, param, xaxis_var, yaxis_var, trt_group, code_data_processing) {

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
      xmin_scale <- min(scale_data[[input$xaxis_var]], na.rm = TRUE)
      xmax_scale <- max(scale_data[[input$xaxis_var]], na.rm = TRUE)

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
    ymin_scale <- min(scale_data[[input$yaxis_var]], na.rm = TRUE)
    ymax_scale <- max(scale_data[[input$yaxis_var]], na.rm = TRUE)

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
    xaxis_var <- input$xaxis_var
    yaxis_var <- input$yaxis_var
    font_size <- input$font_size
    dot_size <- input$dot_size
    reg_text_size <- input$reg_text_size
    hline <- as.numeric(input$hline)
    facet <- input$facet
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
    validate(need(xaxis_var %in% names(ALB),
                  paste("variable", xaxis_var, " is not available in data", dataname)))
        validate(need(yaxis_var %in% names(ALB),
                  paste("variable", yaxis_var, " is not available in data", dataname)))
        
    p <- goshawk:::g_scatterplot(
      data = ALB,
      param_var = param_var,
      param = param,
      xaxis_var = xaxis_var,
      yaxis_var = yaxis_var,
      trt_group = trt_group,
      color_manual = NULL,
      facet = facet,
      reg_line = reg_line,
      xmin_scale = xmin_scale,
      xmax_scale = xmax_scale,
      ymin_scale = ymin_scale,
      ymax_scale = ymax_scale,
      font_size = font_size,
      dot_size = dot_size,
      reg_text_size = reg_text_size,
      rotate_xlab = rotate_xlab,
      hline = hline
    )

    p

  })
    
}
 