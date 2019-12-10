#' Box Plot
#'
#' This teal module renders the UI and calls the functions that create a box plot and accompanying
#' summary table.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured
#'  laboratory data frame ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param list of biomarkers of interest.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL.
#' @param xaxis_var variable to categorize the x-axis.
#' @param facet_var variable to facet the plots by.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param armlabel label for the treatment symbols in the legend. If not specified then the label
#'  attribute for trt_group will be used. If there is no label attribute for trt_group, then the
#'  name of the parameter (in title case) will be used.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position a horizontal line.  NULL = No line.
#' @param plot_height numeric vectors to define the plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param alpha numeric vector to define transparency of plotted points.
#'
#' @inheritParams teal.devel::standard_layout
#'
#' @import DescTools
#' @import utils
#' @import dplyr
#' @import goshawk
#' @import teal
#'
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return an \code{\link[teal]{module}} object#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
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
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'     TRUE ~ as.character(NA)),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)),
#'             AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
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
#'             grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'             TRUE ~ as.character(NA)),
#'           AVISITCDN = case_when(
#'             AVISITCD == "SCR" ~ -2,
#'             AVISITCD == "BL" ~ 0,
#'             grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'             TRUE ~ as.numeric(NA)),
#'           AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
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
#'       tm_g_boxplot(
#'         label = "Box Plot",
#'         dataname = "ADLB",
#'         param_var = "PARAMCD",
#'         param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'         yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
#'         rotate_xlab = FALSE,
#'         xaxis_var = choices_selected(c("ARM", "AVISITCD", "STUDYID"), "ARM"),
#'         facet_var = choices_selected(c("ARM", "AVISITCD", "SEX"), "AVISITCD"),
#'         trt_group = "ARM"
#'       )
#'   )
#' )
#' shinyApp(x$ui, x$server)
#'
#'}


tm_g_boxplot <- function(label,
                         dataname,
                         param_var,
                         param,
                         yaxis_var,
                         xaxis_var,
                         facet_var = choices_selected("ARM", "ARM"),
                         trt_group = "ARM",
                         armlabel = NULL,
                         color_manual = NULL,
                         shape_manual = NULL,
                         facet_ncol = NULL,
                         rotate_xlab = FALSE,
                         hline = NULL,
                         plot_height = c(600, 200, 2000),
                         font_size = c(12, 8, 20),
                         dot_size = c(2, 1, 12),
                         alpha = c(0.8, 0.0, 1.0),
                         pre_output = NULL,
                         post_output = NULL) {

  args <- as.list(environment())

  stopifnot(is.choices_selected(param))
  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))
  stopifnot(is.choices_selected(facet_var))


  #TODO: f/u on reciptrocal actions of xvar and facet_var

  # TODO: where to do this check?
  # If there are no choices specified for treatment group/x axis/fact then set the
  # appropriate choices variable to the treatment group to enable the display of the treatment
  # group variable on the UI.
  # if (is.null(args$xaxis_var_choices)) args$xaxis_var_choices = args$xaxis_var
  # if (is.null(args$facet_var_choices)) args$facet_var_choices = args$facet_var

  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual,
                       armlabel = armlabel
    ),
    ui = ui_g_boxplot,
    ui_args = args
  )
}

ui_g_boxplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = div(
      fluidRow(
        uiOutput(ns("plot_ui"))
      ),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Selected Data Points"),
        DT::dataTableOutput(ns("brush_data"))
      )),
      fluidRow(column(
        width = 12,
        br(), hr(),
        h4("Descriptive Statistics"),
        uiOutput(ns("table_ui"))
      ))
    ),
    encoding =  div(
      templ_ui_dataname(a$dataname),
      templ_ui_param(ns, a$param$choices, a$param$selected),
      templ_ui_xy_vars(ns, a$xaxis_var$choices, a$xaxis_var$selected,
                       a$yaxis_var$choices, a$yaxis_var$selected),
      optionalSelectInput(ns("facet_var"),
                          label = "Facet by",
                          choices = a$facet_var$choices,
                          selected = a$facet_var$selected,
                          multiple = FALSE
      ),
      templ_ui_constraint(ns), # required by constr_anl_chunks
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline),
          sliderInput(ns("yrange_scale"), label = "Y-Axis Range Zoom", min = 0, max = 1, value = c(0, 1))
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
          optionalSliderInputValMinMax(ns("font_size"),  "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("alpha"), "Dot Transparency", a$alpha, ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_boxplot <- function(input,
                          output,
                          session,
                          datasets,
                          dataname,
                          param_var,
                          trt_group,
                          facet_var,
                          color_manual,
                          shape_manual,
                          armlabel){

  ns <- session$ns

  # reused in all modules
  anl_chunks <- constr_anl_chunks(session, input, datasets, dataname, "param", param_var, trt_group)

  # update sliders for axes
  keep_range_slider_updated(session, input, "yrange_scale", "yaxis_var", anl_chunks)

  create_plot <- reactive({
    private_chunks <- anl_chunks()$chunks$clone(deep = TRUE)

    xaxis <- input$xaxis_var
    facet_var <- input$facet_var

    yrange_scale <- input$yrange_scale
    facet_ncol <- input$facet_ncol
    alpha <- input$alpha
    font_size <- input$font_size
    dot_size <- input$dot_size
    rotate_xlab = input$rotate_xlab
    hline <- input$hline

    # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_chunk()) and some inputs
    param <- isolate(input$param)
    yaxis <- isolate(input$yaxis_var)

    chunks_push(
      chunks = private_chunks,
      id = "boxplot",
      expression = bquote({
        plot <- g_boxplot(
          data = ANL,
          biomarker = .(param),
          xaxis_var = .(xaxis),
          yaxis_var = .(yaxis),
          hline = .(`if`(is.na(hline), NULL, as.numeric(hline))),
          facet_ncol = .(facet_ncol),
          rotate_xlab = .(rotate_xlab),
          trt_group = .(trt_group),
          ymin_scale = .(yrange_scale[1]),
          ymax_scale = .(yrange_scale[2]),
          color_manual = .(color_manual),
          shape_manual = .(shape_manual),
          facet = .(facet_var),
          alpha = .(alpha),
          dot_size = .(dot_size),
          font_size = .(font_size),
          armlabel = .(armlabel),
          unit = .("AVALU") # TODO: check fix at goshawk level
        )
      })
    )

    chunks_safe_eval(private_chunks)

    private_chunks
  })

  create_table <- reactive({
    private_chunks <- create_plot()$clone(deep = TRUE)

    param <- input$param
    xaxis_var <- input$yaxis_var
    facet_var <- input$facet_var
    font_size <- input$font_size

    chunks_push(
      chunks = private_chunks,
      id = "table",
      expression = bquote({
        tbl <- t_summarytable(
          data = ANL,
          trt_group = .(trt_group),
          param_var = .(param_var),
          param = .(param),
          xaxis_var = .(xaxis_var),
          visit_var = .("AVISITCD"),  #TODO: should be?  .(facet_var),
          font_size = .(font_size)
        )
      })
    )

    chunks_safe_eval(private_chunks)
    private_chunks
  })

  main_code <- reactive({
    private_chunks <- create_table()
    init_chunks(private_chunks)
    private_chunks
  })

  output$boxplot <- renderPlot({
    main_code()$get("plot")
  })

  output$table_ui <- renderTable({
    main_code()$get("tbl")
  })

  # dynamic plot height and brushing
  output$plot_ui <- renderUI({

    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))

    plotOutput(ns("boxplot"),
               height = plot_height,
               brush = brushOpts(id = ns("boxplot_brush"), resetOnNew = T)
    )
  })

  # highlight plot area
  output$brush_data <- DT::renderDataTable({
    req(input$boxplot_brush)

    ANL <- isolate(anl_chunks()$ANL) # nolint
    validate_has_data(ANL, 5)

    xvar <- isolate(input$xaxis_var)
    yvar <- isolate(input$yaxis_var)
    facetv <- isolate(input$facet_var)

    req(all(c(xvar, yvar, facetv) %in% names(ANL)))

    # TODO: check reactivity
    df <- brushedPoints(
      select(ANL, "USUBJID", trt_group, facetv, "AVISITCD", "PARAMCD", xvar, yvar, "LOQFL"),
      input$boxplot_brush
    )

    numeric_cols <- names(select_if(df, is.numeric))

    DT::datatable(df, rownames = FALSE) %>%
      DT::formatRound(numeric_cols, 4)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Box Plot"
  )


}

srv_g_boxplot_old <- function(input,
                          output,
                          session,
                          datasets,
                          facet_var,
                          facet_var_choices,
                          xaxis_var,
                          xaxis_var_choices,
                          param_var,
                          param,
                          yaxis_var,
                          trt_group,
                          color_manual,
                          shape_manual,
                          armlabel,
                          filter_vars,
                          filter_labs,
                          dataname) {

  ns <- session$ns

  #TODO: check how this is changed.
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

  #TODO: check how this is changed.
  # Extend is.infinite to include zero length objects.
  is_finite <- function(x){
    if(length(x) == 0) return(FALSE)
    return(is.finite(x))
  }

  #TODO: check how this is changed.
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

    plotOutput(ns("boxplot"), height = plot_height,
               brush = brushOpts(id = ns("boxplot_brush"), resetOnNew=T)
    )
  })

  #TODO: in ref this is commented out
  output$brush_data <- renderTable({
    if (nrow(filter_ALB()) > 0 ){

      ##--- identify brushed subset of the mtcars data.frame without brushedPoints
      req(input$boxplot_brush)

      # I use shorter variable names for better readability
      facet.var <- input$boxplot_brush$mapping$panelvar1  # column used for faceting
      x.axis.var <- input$boxplot_brush$mapping$x  # column used for x-axis
      y.axis.var <- input$boxplot_brush$mapping$y  # column used fo y-axis
      facet.value <- input$boxplot_brush$panelvar1  # level of the brushed facet

      # First, subset the data.frame to those rows that match the brushed facet level
      datfilt <- select(filter_ALB(), "USUBJID", trt_group, input$boxplot_brush$mapping$panelvar1, "AVISITCD", "PARAMCD",
                        input$xaxis_var, input$yaxis_var, "LOQFL") %>%
        droplevels() %>%
        filter_at(input$facet_var, all_vars(.data== facet.value) )

      if (!(is.factor(datfilt[[x.axis.var]])| is.numeric(datfilt[[x.axis.var]]))){
        datfilt[[x.axis.var]] <- as.factor(datfilt[[x.axis.var]])
      }

      if (!(is.factor(datfilt[[y.axis.var]])| is.numeric(datfilt[[y.axis.var]]))){
        datfilt[[y.axis.var]] <- as.factor(datfilt[[y.axis.var]])
      }

      # Finally, within this facet, identify points within the brushed ranges
      datfilt %>%
        filter(
          # interpret the factor levels as integers, to match how ggplot2
          # places them on the axes. The 'droplevels' call above ensures that
          # only levels that are present in the current facet are matched
          as.integer(datfilt[[x.axis.var]]) < input$boxplot_brush$xmax,
          as.integer(datfilt[[x.axis.var]]) > input$boxplot_brush$xmin,
          datfilt[[y.axis.var]] < input$boxplot_brush$ymax,
          datfilt[[y.axis.var]] > input$boxplot_brush$ymin
        ) %>%
        arrange_at(
          input$xaxis_var
        )



    } else{
      NULL
    }
  })

  # filter data by param and the xmin and xmax values from the filter slider.
  filter_ALB <- reactive({


    param <- input$param
    yaxis_var <- input$yaxis_var

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
      rng <- range(cdata()[[input$yaxis_var]], na.rm = TRUE)

    return(list(low = rng[1], high = rng[2]))
  })

  # dynamic slider for y-axis - Use ylimits
  observe({


    if (input$y_filter_by == "None") {
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
          sliderInput(session$ns("yrange_scale"),
                      label = paste0("Y-Axis Range Zoom"),
                      min = yax$min,
                      max = yax$max,
                      step = yax$step,
                      value = c(yax$min, yax$max)
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
    yaxis_var <- input$yaxis_var
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
    ASL_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
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
    validate(need(yaxis_var %in% names(ALB),
                  paste("Variable", yaxis_var, " is not available in data", dataname)))
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

    # re-establish treatment variable label
    if (trt_group == "ARM"){
      attributes(ALB_FILTERED$ARM)$label <- "Planned Arm"
    } else {
      attributes(ALB_FILTERED$ACTARM)$label <- "Actual Arm"
    }

    chunks$analysis <<- call(
      "g_boxplot",
      data = bquote(.(as.name(data_name))),
      biomarker = param,
      yaxis_var = yaxis_var,
      hline = hline,
      facet_ncol = facet_ncol,
      rotate_xlab = rotate_xlab,
      trt_group = trt_group,
      unit = unit,
      ymin_scale = ymin_scale,
      ymax_scale = ymax_scale,
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
    validate(need(nrow(ALB) > 0 , ""))

    param <- input$param
    xaxis_var <- input$yaxis_var
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

}
