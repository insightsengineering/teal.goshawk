#' #' Scatter Plot
#'
#' This teal module renders the UI and calls the function that creates a scatter plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured 
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker selected.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in 
#' visualization. Use with facet TRUE.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @inheritParams teal.devel::standard_layout
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#'
#' @details This module displays a scatter plot. link to specification file \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#' library(random.cdisc.data)
#' 
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD",
#'                     "B: Placebo" = "Placebo", 
#'                     "C: Combination" = "Combination")
#' 
#' ADSL <- radsl(N = 20, seed = 1)
#' ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL", 
#'       grepl("WEEK", AVISIT) ~ gsub("^WEEK ([0-9]+) .+$" , "W \\1", AVISIT),
#'       TRUE ~ as.character(NA)),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0, 
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
#' 
#' 
#' x <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = {'
#'       arm_mapping <- list("A: Drug X" = "150mg QD", 
#'                           "B: Placebo" = "Placebo",
#'                           "C: Combination" = "Combination")
#' 
#'       ADSL <- radsl(N = 20, seed = 1)
#'       ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#'       ADLB <- ADLB %>%
#'         mutate(AVISITCD = case_when(
#'             AVISIT == "SCREENING" ~ "SCR",
#'             AVISIT == "BASELINE" ~ "BL", 
#'             grepl("WEEK", AVISIT) ~ gsub("^WEEK ([0-9]+) .+$" , "W \\1", AVISIT),
#'             TRUE ~ as.character(NA)),
#'           AVISITCDN = case_when(
#'             AVISITCD == "SCR" ~ -2,
#'             AVISITCD == "BL" ~ 0, 
#'             grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'             TRUE ~ as.numeric(NA)),
#'           TRTORD = case_when(
#'             ARMCD == "ARM C" ~ 1,
#'             ARMCD == "ARM B" ~ 2,
#'             ARMCD == "ARM A" ~ 3),
#'           ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'           ARM = factor(ARM) %>% reorder(TRTORD))
#'           '},
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'        label = "Scatter Plot",
#'        dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'        xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG", "AVAL2"), "BASE"),
#'        yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG", "AVAL2"), "AVAL"),
#'        trt_group = "ARM",
#'        color_manual = c("150mg QD" = "#000000", 
#'                         "Placebo" = "#3498DB",
#'                         "Combination" = "#E74C3C"),
#'        shape_manual = c("N"  = 1, "Y"  = 2, "NA" = 0),
#'        plot_height = c(500, 200, 2000),
#'        facet_ncol = 2,
#'        facet = FALSE,
#'        facet_var = "ARM",
#'        reg_line = FALSE,
#'        font_size = c(12, 8, 20),
#'        dot_size = c(1, 1, 12),
#'        reg_text_size = c(3, 3, 10)
#'    )
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)
#'}

tm_g_scatterplot <- function(
  label,
  dataname,
  param_var,
  param,
  xaxis_var,
  yaxis_var,
  trt_group = "ARM",
  color_manual = NULL,
  shape_manual = NULL,
  facet_ncol = 2,
  facet = FALSE,
  facet_var = "ARM",
  reg_line = FALSE,
  rotate_xlab = FALSE,
  hline = NULL,
  vline = NULL,
  plot_height = c(500, 200, 2000),
  font_size = c(12, 8, 20),
  dot_size = c(1, 1, 12),
  reg_text_size = c(3, 3, 10),
  pre_output = helpText("graph needs to be of a certain width to be displayed"),
  post_output = NULL) {
  
  stopifnot(is.choices_selected(param))
  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = dataname,
    server = srv_g_scatterplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual
    ),
    ui = ui_g_scatterplot,
    ui_args = args
  )
  
}

ui_g_scatterplot <- function(id, ...) {
  
  ns <- NS(id)
  a <- list(...)
  
  standard_layout(
    output = div(
      fluidRow(
        uiOutput(ns("plot_ui"))
      ),
      fluidRow(
        column(width = 12,
               br(), hr(),
               h4("Selected Data Points"),
               tableOutput(ns("brush_data"))
        )
      )
    ),
    encoding =  div(
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      optionalSelectInput(ns("param"), 
                          "Select a Biomarker",
                          a$param$choices,
                          a$param$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("xaxis_var"), 
                          "Select an X-Axis Variable", 
                          a$xaxis_var$choices, 
                          a$xaxis_var$selected, 
                          multiple = FALSE),
      optionalSelectInput(ns("yaxis_var"),
                          "Select a Y-Axis Variable", 
                          a$yaxis_var$choices,
                          a$yaxis_var$selected,
                          multiple = FALSE),
      radioButtons(ns("constraint_var"), 
                   "Data Constraint", 
                   c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")),
      uiOutput(ns("constraint_range"), style="display: inline-block; vertical-align:center"),
      tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;"),
      uiOutput(ns("xaxis_zoom")),
      uiOutput(ns("yaxis_zoom")),
      numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
      checkboxInput(ns("facet"), "Treatment Facetting", a$facet),
      checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
      checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      numericInput(ns("vline"), "Add a vertical line:", a$vline),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("plot_height"), 
                                       "Plot Height", 
                                       a$plot_height, 
                                       ticks = FALSE),
          optionalSliderInputValMinMax(ns("font_size"), 
                                       "Font Size",
                                       a$font_size, 
                                       ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"),
                                       "Dot Size",
                                       a$dot_size,
                                       ticks = FALSE),
          optionalSliderInputValMinMax(ns("reg_text_size"), 
                                       "Regression Annotations Size", 
                                       a$reg_text_size, 
                                       ticks = FALSE)
        )
      )
    ),
    forms = tags$div(
      actionButton(ns("show_rcode"), 
                   "Show R Code", 
                   width = "100%")
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
  
}

srv_g_scatterplot <- function(input, output, session, datasets, dataname, 
                              param_var, trt_group, facet_var, color_manual, shape_manual) {
  
  ns <- session$ns
  init_chunks()
  dataset_var <- paste0(dataname, "_FILTERED")
  values <- reactiveValues(curr_constr = "NONE")
  
  # min/max data constraint value an xlim, ylim
  observeEvent(input$constraint_var, {
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      dplyr::filter_(sprintf("%s == '%s'", param_var, input$param))
    
    constraint_var <- input$constraint_var
    if (constraint_var != "NONE") {
      visitFreq <- unique(ANL$AVISITCD)
      if ((constraint_var == "BASE2" & any(grepl("SCR", visitFreq))) ||  
          (constraint_var == "BASE" & any(grepl("BL", visitFreq)))) {
        
        constraint_min_range <- floor(min(ANL[[constraint_var]], na.rm = TRUE) * 1000) / 1000
        constraint_max_range <- ceiling(max(ANL[[constraint_var]], na.rm = TRUE) * 1000) / 1000
        
        output$constraint_range <- renderUI({
          shinyWidgets::numericRangeInput(
            ns("constraint_range"), 
            sprintf("Range of '%s' [%s-%s]", constraint_var, constraint_min_range, constraint_max_range), 
            c(constraint_min_range, constraint_max_range), 
            width = NULL, separator = " to ")
        })
      }
      values$curr_constr <- constraint_var
    } else {
      output$constraint_range <- renderUI(NULL)
      values$curr_constr <- constraint_var
    }
  })
  
  # filters
  get_data <- reactive({
    constraint_var <- isolate(input$constraint_var)
    req((constraint_var == "NONE" || !is.null(input$constraint_range)) &&
        constraint_var == values$curr_constr)
    
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)))
    
    # filter biomarker
    param <- input$param
    chunks_push(
      id = "filter_biomarker",
      bquote({
        ANL <- .(as.name(dataset_var)) %>%
          dplyr::filter(.(as.name(param_var)) == .(param))
      }))
    chunks_safe_eval()
    
    # filter constraint
    ANL <- chunks_get_var("ANL")
    if (constraint_var != "NONE") {
      if ((floor(min(ANL[[constraint_var]], na.rm = TRUE) * 1000) / 1000) < input$constraint_range[1] ||
          (ceiling(max(ANL[[constraint_var]], na.rm = TRUE) * 1000) / 1000) > input$constraint_range[2]) {
        chunks_push(
          id = "filter_constraint",
          bquote({
            ANL <- ANL %>%
              dplyr::filter(
                (.(input$constraint_range[1]) <= .(as.name(constraint_var)) &&
                 .(as.name(constraint_var)) <= .(input$constraint_range[2])) ||
                is.na(.(as.name(constraint_var)))
              )
          }))
        chunks_safe_eval()
      }
    }
    
    # Check data size and add attributes
    ANL <- chunks_get_var("ANL")
    validate_has_data(ANL, 3)
    
    if (trt_group == "ARM") {
      chunks_push(id = "change_attr", bquote(attributes(ANL$ARM)$label <- "Planned Arm"))
    } else {
      chunks_push(id = "change_attr", bquote(attributes(ANL$ACTARM)$label <- "Actual Arm"))
    }
    chunks_push_new_line()
    
    return(chunks_get_var("ANL"))
  })
  
  # dynamic slider for axes
  output$xaxis_zoom <- renderUI({
    ANL <- get_data()
    req(ANL, cancelOutput = TRUE)
    xmin_scale <- min(ANL[[input$xaxis_var]], na.rm = TRUE)
    xmax_scale <- max(ANL[[input$xaxis_var]], na.rm = TRUE)
    
    tagList({
      optionalSliderInput(ns("xrange_scale"), 
                          label = "X-Axis Range Zoom", 
                          min = floor(xmin_scale), 
                          max = ceiling(xmax_scale), 
                          value = c(floor(xmin_scale), ceiling(xmax_scale)))
    })
  })
  output$yaxis_zoom <- renderUI({
    ANL <- get_data()
    ymin_scale <- min(ANL[[input$yaxis_var]], na.rm = TRUE)
    ymax_scale <- max(ANL[[input$yaxis_var]], na.rm = TRUE)
    
    tagList({
      optionalSliderInput(ns("yrange_scale"), 
                          label = "Y-Axis Range Zoom", 
                          min = floor(ymin_scale), 
                          max = ceiling(ymax_scale), 
                          value = c(floor(ymin_scale), ceiling(ymax_scale)))
    })    
  })
  
  # plot 
  output$scatterplot <- renderPlot({
    ANL <- get_data()
    isolate({
      param <- input$param
      xaxis <- input$xaxis_var
      yaxis <- input$yaxis_var
    })
    chunks_push(
      bquote({
        # re-establish treatment variable label
        g_scatterplot(
          data = ANL,
          param_var = .(param_var),
          param = .(param),
          xaxis_var = .(xaxis),
          yaxis_var = .(yaxis),
          trt_group = .(trt_group),
          xmin = .(input$xrange_scale[1]),
          xmax = .(input$xrange_scale[2]),
          ymin = .(input$yrange_scale[1]),
          ymax = .(input$yrange_scale[2]),
          color_manual = .(color_manual),
          shape_manual = .(shape_manual),
          facet_ncol = .(input$facet_ncol),
          facet = .(input$facet),
          facet_var = .(facet_var),
          reg_line = .(input$reg_line),
          font_size = .(input$font_size),
          dot_size = .(input$dot_size),
          reg_text_size = .(input$reg_text_size),
          rotate_xlab = .(input$rotate_xlab),
          hline = .(as.numeric(input$hline)),
          vline = .(as.numeric(input$vline))      
        )
      }))
    
    # evaluate the code chunk so that it is available in app environment as well
    chunks_safe_eval()
  })
  
  # dynamic plot height and brushing
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(ns("scatterplot"), height = plot_height,
               brush = brushOpts(id = ns("scatterplot_brush"), resetOnNew=T)
    )
  })
  
  # highlight plot area
  output$brush_data <- renderTable({
    ANL <- get_data()
    if (!is.null(ANL) && nrow(ANL) > 0 ){
      brushedPoints(select(ANL, "USUBJID", trt_group, "AVISITCD", "PARAMCD", 
                           input$xaxis_var, input$yaxis_var, "LOQFL"), 
                    input$scatterplot_brush)
    } else{
      NULL
    }
  })

  observeEvent(input$show_rcode, {
    chunks_ids <- get_chunks_object()$.__enclos_env__$private$id
    show_rcode_modal(
      title = "Scatter Plot", 
      rcode = get_rcode(
        datasets = datasets, 
        selected_chunk_ids = chunks_ids[c(grep("filter|attr", chunks_ids), length(chunks_ids))], 
        title = "Scatter Plot"
      )
    )
  })
  
}
