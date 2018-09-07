tm_g_spaghettiplot <- function(label,
                               dataname,
                               idvar,
                               xvar, yvar,
                               xvar_choices = xvar, yvar_choices = yvar,
                               # xvar_level = NULL,
                               param_var,
                               param, param_choices = param,
                               trt_group,
                               # trt_group_level = NULL,
                               # hline = NULL,
                               # man_color = NULL,
                               # rotate_xlab = FALSE,
                               facet_ncol = 2,
                               plot_height = c(600, 200, 2000)) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_lineplot,
    server_args = list(dataname = dataname, idvar = idvar, param_var = param_var, trt_group = trt_group),
    ui = ui_spaghettiplot,
    ui_args = args,
    filters = dataname
  )
  
}


ui_spaghettiplot <- function(id, ...) {
  
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
      numericInput(ns("facet_ncol"), "Select number of plots per row:", a$facet_ncol, min = 1),
      
      if (all(c(
        length(a$plot_height) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;")
      },
      # checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
      # numericInput(ns("hline"), "Add a horizontal line:", a$hline),
      uiOutput(ns("yaxis_scale")),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
  
}

srv_lineplot <- function(input, output, session, datasets, dataname, idvar, param_var, trt_group) {
  
  ns <- session$ns
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("spaghettiplot"), height=plot_height)
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
  
  output$spaghettiplot <- renderPlot({
    
    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    param <- input$param
    xvar <- input$xvar
    yvar <- input$yvar
    ymin_scale <- input$yrange_scale[1]
    ymax_scale <- input$yrange_scale[2]
    facet_ncol <- input$facet_ncol
    # rotate_xlab <- input$rotate_xlab
    # hline <- as.numeric(input$hline)
    
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
      "g_spaghettiplot",
      data = bquote(.(as.name(data_name))),
      subj_id = idvar,
      biomarker_var = param_var,
      biomarker = param,
      value_var = yvar,
      trt_group = trt_group,
      time = xvar,
      ymin = ymin_scale,
      ymax = ymax_scale,
      facet_ncol = facet_ncol
    )
    
    p <- try(eval(chunks$analysis))
    
    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create the line plot:\n\n", p)))
    
    p
    
  })
  
  observeEvent(input$show_rcode, {
    
    header <- teal.tern:::get_rcode_header(
      title = "Spaghetti Plot",
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
      title = "R Code for the Current Spaghetti Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
}
