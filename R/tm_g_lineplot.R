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
#' @param param parameter name
#' @param param_choices vector of parameter names that can be used in param.
#' @param trt_group single name of treatment arm variable.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param stat string of statistics
#' @param hline numeric value to add horizontal line to plot
#' @param man_color vector of strings or numeric values that representing colors
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels
#' @param plot_height numeric vectors to define the plot height.
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
#' # EXAMPLE 1
#' 
#' # Example using analysis dataset for example ASL or ADSL,
#' library(goshawk)
#' library(DescTools)
#' library(teal)
#' 
#' # ALB points to biomarker data stored in a typical LB structure. for example ALB or ADLB.
#' 
#' # for development team testing
#' ASL_path <- "~/btk/lupus/dataadam/asl.sas7bdat"
#' ALB_path <- "~/btk/lupus/dataadam/alb3arm.sas7bdat"
#' 
#' # list of biomarkers of interest. see ALB2 assignment below
#' param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")
#' 
#' ASL0 <- read_bce(ASL_path)
#' ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')
#' 
#' ALB0 <- read_bce(ALB_path)
#' 
#' # post process the data to subset records per specification
#' ALB_SUBSET <- subset(ALB0,
#'                      subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'),
#'                      select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
#'                                 'LBSTRESC', 'LBSTRESN'))
#' 
#' # calculate the minimum AVAL for each PARAMCD
#' PARAM_MINS <- ALB_SUBSET %>%
#'   select(USUBJID, PARAMCD, AVAL) %>%
#'   filter(PARAMCD %in% param_choices) %>%
#'   group_by(PARAMCD) %>%
#'   summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))
#' 
#' # post process the data to create several new variables and adjust existing record specific valules per specification
#' # - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
#' # - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
#' ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1),
#'                                                       substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'   mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4))) %>%
#'   mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
#'   mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
#'   mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG))
#' # may need to add similar code for BASE2 related variables
#' 
#' 
#' # merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
#' ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
#'   mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
#'   mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
#' #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))
#' 
#' # for proper chronological ordering of visits in visualizations
#' ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
#' ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#' 
#' # to test loq_flag
#' ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ALB = ALB),
#'   modules = root_modules(
#'     tm_g_lineplot(
#'       label = "Line Plot",
#'       dataname = "ALB",
#'       xvar = "AVISITCD",
#'       yvar = "AVAL",
#'       yvar_choices = c("AVAL","CHG","PCGH"),
#'       param_var = "PARAMCD",
#'       param = "CRP",
#'       param_choices = param_choices,
#'       trt_group = "ARM"
#'     )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)
#' 
#' # EXAMPLE 2
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
                          param_var,
                          param, param_choices = param,
                          trt_group,
                          trt_group_level = NULL,
                          stat = "mean",
                          hline = NULL,
                          man_color = NULL,
                          rotate_xlab = FALSE,
                          plot_height = c(600, 200, 2000)) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_lineplot,
    server_args = list(dataname = dataname, param_var = param_var, trt_group = trt_group, man_color = man_color, 
                       xvar_level = xvar_level, trt_group_level = trt_group_level),
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
      uiOutput(ns("yaxis_scale")),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
  
}

srv_lineplot <- function(input, output, session, datasets, dataname, param_var, trt_group, man_color, xvar_level, trt_group_level) {
  
  ns <- session$ns
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("lineplot"), height=plot_height)
  })
  
  # dynamic slider for y-axis
  output$yaxis_scale <- renderUI({
    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param 
    scale_data <- ANL %>%
      filter(eval(parse(text = param_var)) == param)
    
    # identify min and max values of BM range ignoring NA values
    ymin_scale <- min(scale_data[[input$yvar]], na.rm = TRUE)
    ymax_scale <- max(scale_data[[input$yvar]], na.rm = TRUE)
    
    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Scale", ymin_scale, ymax_scale, value = c(ymin_scale, ymax_scale))
    })
    
  })
  
  chunks <- list(
    analysis = "# Not Calculated"
  )
  
  output$lineplot <- renderPlot({
    
    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param
    xvar <- input$xvar
    yvar <- input$yvar
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    rotate_xlab <- input$rotate_xlab
    hline <- as.numeric(input$hline)
    
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
      rotate_xlab = rotate_xlab
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
