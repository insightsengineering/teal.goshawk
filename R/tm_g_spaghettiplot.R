#' Spaghetti Plot
#'
#' This teal module renders the UI and calls the function that creates a spaghetti plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param_choices list of biomarkers of interest.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data that includes parameter labels.
#' @param idvar name of unique subject id variable.
#' @param xvar single name of variable in analysis data that is used as x-axis in the plot for the
#' respective goshawk function.
#' @param xvar_choices vector with variable names that can be used as xvar.
#' @param xvar_level vector that can be used to define the factor level of xvar. Only use it when
#' xvar is character or factor.
#' @param filter_var data constraint variable.
#' @param filter_var_choices data constraint variable choices.
#' @param yvar single name of variable in analysis data that is used as summary variable in the
#' respective gshawk function.
#' @param yvar_choices vector with variable names that can be used as yvar.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param man_color string vector representing customized colors
#' @param color_comb name or hex value for combined treatment color.
#' @param hline numeric value to add horizontal line to plot
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric.
#' Default value is waive().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values. Default
#'  value is waive().
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param plot_height numeric vectors to define the plot height.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param group_stats control group mean or median overlay.
#'
#' @import goshawk
#'
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return \code{shiny} object
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
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
#'             grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
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
#'     tm_g_spaghettiplot(
#'       label = "Spaghetti Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       idvar = "USUBJID",
#'       xvar = choices_selected(c("AVISITCD", "AVISITCD")),
#'       yvar = choices_selected(c("AVAL","CHG", "PCHG"), "AVAL"),
#'       filter_var = choices_selected(c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE"), "NONE"),
#'       trt_group = "ARM",
#'       color_comb = "#39ff14"
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }

tm_g_spaghettiplot <- function(label,
                               dataname,
                               param_var,
                               param,
                               param_var_label = 'PARAM',
                               idvar,
                               xvar,
                               yvar,
                               xvar_level = NULL,
                               filter_var = yvar,
                               trt_group,
                               trt_group_level = NULL,
                               group_stats = "NONE",
                               hline = NULL,
                               man_color = NULL,
                               color_comb = NULL,
                               xtick = waiver(), xlabel = xtick,
                               rotate_xlab = FALSE,
                               facet_ncol = 2,
                               plot_height = c(600, 200, 2000),
                               font_size = c(12, 8, 20)) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_spaghettiplot,
    server_args = list(dataname = dataname,
                       idvar = idvar,
                       param_var = param_var,
                       trt_group = trt_group,
                       yvar = yvar$selected,
                       xvar_level = xvar_level,
                       trt_group_level = trt_group_level,
                       man_color = man_color,
                       color_comb = color_comb,
                       param_var_label = param_var_label,
                       xtick = xtick,
                       xlabel = xlabel),
    ui = g_ui_spaghettiplot,
    ui_args = args,
    filters = dataname
  )

}

g_ui_spaghettiplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  if (a$plot_height < 200 || a$plot_height > 2000) stop("plot_height must be between 200 and 2000")

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label(a$dataname, "Data Settings", class="text-primary"),
      optionalSelectInput(ns("param"),
                          "Select a Biomarker",
                          a$param$choices,
                          a$param$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("xvar"),
                          "X-Axis Variable",
                          a$xvar$choices,
                          a$xvar$selected,
                          multiple = FALSE),
      optionalSelectInput(ns("yvar"),
                          "Select a Y-Axis Variable",
                          a$yvar$choices,
                          a$yvar$selected,
                          multiple = FALSE),
      radioButtons(ns("group_stats"),
                   "Group Statistics",
                   c("None" = "NONE", "Mean" = "MEAN", "Median" = "MEDIAN"),
                   inline = TRUE),
      radioButtons(ns("filter_var"),
                   "Data Constraint",
                   a$filter_var$choices,
                   a$filter_var$selected),
      uiOutput(ns("filter_min"),
               style="display: inline-block; vertical-align:center"),
      uiOutput(ns("filter_max"),
               style="display: inline-block; vertical-align:center"),
      uiOutput(ns("yaxis_scale")),

      if (all(c(
        length(a$plot_height) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Aesthetic Settings", class="text-primary", style="margin-top: 15px;")
      },
      div(style="padding: 0px;",
          div(style="display: inline-block;vertical-align:moddle; width: 175px;",
              tags$b("Number of Plots Per Row:")),
          div(style="display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("facet_ncol"), "", a$facet_ncol, min = 1))
      ),
      checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
      div(style="padding: 0px;",
          div(style="display: inline-block;vertical-align:moddle; width: 175px;",
              tags$b("Add a Horizontal Line:")),
          div(style="display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("hline"), "", a$hline))
      ),
      optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "Line Transparency", a$alpha, value_min_max =  c(0.8, 0.0, 1.0), step = 0.1, ticks = FALSE)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_spaghettiplot <- function(input, output, session, datasets, dataname, idvar, param_var, trt_group, man_color,
                              color_comb, yvar, xvar_level, trt_group_level, param_var_label, xtick, xlabel) {

  ns <- session$ns

  ## dynamic plot height and brushing
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))

    plotOutput(ns("spaghettiplot"), height=plot_height)
    # brush = brushOpts(id = ns("spaghettiplot_brush"))
    # )
  })

  # output$brush_data <- renderDataTable({
  #   # brush_results <- brushedPoints(select(filter_ANL(), "USUBJID", "ARM", "AVISITCD", "PARAMCD", yvar, "LOQFL"), input$spaghettiplot_brush)
  #   brush_results <- brushedPoints(select(filter_ANL(), "USUBJID", "ARM"), input$spaghettiplot_brush)
  #   if (nrow(brush_results) > 0) {
  #     datatable(na.omit(brush_results))
  #   }  else {
  #     NULL
  #   }
  # })

  # filter data by param and the y-axis range values
  filter_ANL <- reactive({

    param <- input$param
    filter_var <- input$filter_var
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) %>%
      filter(eval(parse(text = param_var)) == param )

    ymin_scale <- -Inf
    ymax_scale <- Inf

    if(filter_var != "NONE"){
      if (length(input$filtermin)){
        ymin_scale <- input$filtermin
      }

      if (length(input$filtermax)){
        ymax_scale <- input$filtermax
      }

      ANL1 <- ANL %>%
        filter((ymin_scale <= eval(parse(text = filter_var)) &
                  eval(parse(text = filter_var)) <= ymax_scale) |
                 (is.na(filter_var)))

      return(ANL1)
    } else {
      return(ANL)
    }
  })


  # Filter data based on input filter_var
  observe({
    # derive min max value of input filter_var
    ANL <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    param <- input$param
    value_var <- input$filter_var
    scale_data <- filter(ANL, eval(parse(text = param_var)) == param)

    if(value_var == "NONE"){
      output$filter_max <- NULL
      output$filter_min <- NULL
      output$filter_val_scale <- NULL
    } else {
      # identify min and max values of BM range ignoring NA values
      min_scale <- min(scale_data[,value_var], na.rm = TRUE)
      max_scale <- max(scale_data[,value_var], na.rm = TRUE)

      # Output variable UI
      output$filter_min <- renderUI({
        tagList({
          numericInput(session$ns("filtermin"), label = paste0("Min (", min_scale, ")"), value = min_scale, min = min_scale, max = max_scale)
        })
      })

      output$filter_max <- renderUI({
        tagList({
          numericInput(session$ns("filtermax"), label = paste0("Min (", max_scale, ")"), value = max_scale, min = min_scale, max = max_scale)
        })
      })

      output$filter_val_scale <- renderUI({
        tagList({
          sliderInput(ns("filter_scale"), label=paste0("Select Data for ", value_var),
                      floor(min_scale), ceiling(max_scale),
                      value = c(floor(min_scale), ceiling(max_scale)))
        })
      })
    }
  })

  # dynamic slider for y-axis
  output$yaxis_scale <- renderUI({
    ANL <- filter_ANL()

    ymin_scale <- -Inf
    ymax_scale <- Inf

    # identify min and max values of BM range ignoring NA values
    ymin_scale <- min(ANL[[input$yvar]], na.rm = TRUE)
    ymax_scale <- max(ANL[[input$yvar]], na.rm = TRUE)

    tagList({
      sliderInput(ns("yrange_scale"), label="Y-Axis Range Zoom",
                  floor(ymin_scale), ceiling(ymax_scale),
                  value = c(floor(ymin_scale), ceiling(ymax_scale)))
    })

  })

  chunks <- list(
    analysis = "# Not Calculated"
  )

  output$spaghettiplot <- renderPlot({

    ANL <- filter_ANL()
    param <- input$param
    xvar <- input$xvar
    yvar <- input$yvar
    ylim <- input$yrange_scale
    facet_ncol <- input$facet_ncol
    rotate_xlab <- input$rotate_xlab
    hline <- as.numeric(input$hline)
    group_stats <- input$group_stats
    font_size <- input$font_size
    alpha <- input$alpha


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

    # re-establish treatment variable label
    if (trt_group == "ARM"){
      attributes(ANL$ARM)$label <- "Planned Arm"
    } else {
      attributes(ANL$ACTARM)$label <- "Actual Arm"
    }

    chunks$analysis <<- call(
      "g_spaghettiplot",
      data = bquote(.(as.name(data_name))),
      subj_id = idvar,
      biomarker_var = param_var,
      biomarker_var_label = param_var_label,
      biomarker = param,
      value_var = yvar,
      trt_group = trt_group,
      trt_group_level = trt_group_level,
      time = xvar,
      time_level = xvar_level,
      color_manual = man_color,
      color_comb = color_comb,
      ylim = ylim,
      facet_ncol = facet_ncol,
      hline = hline,
      xtick = xtick,
      xlabel = xlabel,
      rotate_xlab = rotate_xlab,
      font_size = font_size,
      alpha = alpha,
      group_stats = group_stats
    )

    p <- try(eval(chunks$analysis))

    if (is(p, "try-error")) validate(need(FALSE, paste0("could not create the line plot:\n\n", p)))

    p

  })

}
