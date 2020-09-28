#' Scatter Plot Teal Module For Biomarker Analysis
#'
#' @description Scatter Plot Teal Module For Biomarker Analysis
#'
#' @inheritParams teal.devel::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured laboratory data frame
#'   \code{ADLB}.
#' @param param_var name of variable containing biomarker codes e.g. \code{PARAMCD}.
#' @param xaxis_param biomarker selected for x-axis.
#' @param yaxis_param biomarker selected for y-axis.
#' @param xaxis_var name of variable containing biomarker results displayed on x-axis e.g. \code{BASE}.
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. \code{AVAL}.
#' @param trt_group name of variable representing treatment group e.g. \code{ARM}.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param visit_facet visit facet toggle.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet
#'   TRUE.
#' @param loq_legend loq legend toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param plot_height controls plot height.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @export
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "Drug X 100mg",
#'                     "B: Placebo" = "Placebo",
#'                     "C: Combination" = "Combination 100mg"
#' )
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = ' # nolint
#'       arm_mapping <- list("A: Drug X" = "Drug X 100mg",
#'                           "B: Placebo" = "Placebo",
#'                           "C: Combination" = "Combination 100mg")
#'
#'       ADSL <- radsl(cached = TRUE)
#'       ADLB <- radlb(cached = TRUE)
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
#'             AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'           TRTORD = case_when(
#'             ARMCD == "ARM C" ~ 1,
#'             ARMCD == "ARM B" ~ 2,
#'             ARMCD == "ARM A" ~ 3),
#'           ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'           ARM = factor(ARM) %>% reorder(TRTORD))
#'           ',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_gh_correlationplot(
#'        label = "Correlation Plot",
#'        dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        xaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'        yaxis_param = choices_selected(c("ALT", "CRP", "IGA"), "CRP"),
#'        xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "BASE"),
#'        yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'        trt_group = "ARM",
#'        color_manual = c("Drug X 100mg" = "#000000",
#'                         "Placebo" = "#3498DB",
#'                         "Combination 100mg" = "#E74C3C"),
#'        shape_manual = c("N"  = 1, "Y"  = 2, "NA" = 0),
#'        plot_height = c(500, 200, 2000),
#'        facet_ncol = 2,
#'        visit_facet = TRUE,
#'        facet = FALSE,
#'        facet_var = "ARM",
#'        reg_line = FALSE,
#'        loq_legend = TRUE,
#'        font_size = c(12, 8, 20),
#'        dot_size = c(1, 1, 12),
#'        reg_text_size = c(3, 3, 10)
#'    )
#'   )
#' )
#'
#' shinyApp(app$ui, app$server)
#'
#' }
tm_g_gh_correlationplot <- function(label,
                                    dataname,
                                    param_var = "PARAMCD",
                                    xaxis_param = "ALT",
                                    xaxis_var = "BASE",
                                    yaxis_param = "CRP",
                                    yaxis_var = "AVAL",
                                    trt_group = "ARM",
                                    color_manual = NULL,
                                    shape_manual = NULL,
                                    facet_ncol = 2,
                                    visit_facet = TRUE,
                                    facet = FALSE,
                                    facet_var = "ARM",
                                    reg_line = FALSE,
                                    loq_legend = TRUE,
                                    rotate_xlab = FALSE,
                                    hline = NULL,
                                    vline = NULL,
                                    plot_height = c(500, 200, 2000),
                                    font_size = c(12, 8, 20),
                                    dot_size = c(1, 1, 12),
                                    reg_text_size = c(3, 3, 10),
                                    pre_output = NULL,
                                    post_output = NULL) {

  stopifnot(is.choices_selected(xaxis_param))
  stopifnot(is.choices_selected(yaxis_param))

  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_correlationplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group,
                       facet_var = facet_var,
                       color_manual = color_manual,
                       shape_manual = shape_manual
    ),
    ui = ui_g_correlationplot,
    ui_args = args
  )

}

#' @importFrom shinyjs hidden
ui_g_correlationplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding =  div(
      templ_ui_dataname(a$dataname),
      templ_ui_params_vars(
        ns,
        xparam_choices = a$xaxis_param$choices, xparam_selected = a$xaxis_param$selected,
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected,
        yparam_choices = a$yaxis_param$choices, yparam_selected = a$yaxis_param$selected,
        ychoices = a$yaxis_var$choices, yselected = a$yaxis_var$selected
      ),
      templ_ui_constraint(ns, "X-Axis Data Constraint"), # required by constr_anl_chunks
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("xrange_scale"), label = "X-Axis Range Zoom",
            min = -1000000, max = 1000000, value = c(-1000000, 1000000)),
          toggle_slider_ui(
            ns("yrange_scale"), label = "Y-Axis Range Zoom",
            min = -1000000, max = 1000000, value = c(-1000000, 1000000)),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("visit_facet"), "Visit Facetting", a$visit_facet),
          checkboxInput(ns("facet"), "Treatment Facetting", a$facet),
          checkboxInput(ns("reg_line"), "Regression Line", a$reg_line),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", a$loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline),
          numericInput(ns("vline"), "Add a vertical line:", a$vline)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
          optionalSliderInputValMinMax(ns("font_size"),  "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("reg_text_size"), "Regression Annotations Size", a$reg_text_size,
                                       ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

#' @importFrom goshawk g_correlationplot
srv_g_correlationplot <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname,
                                  param_var,
                                  trt_group,
                                  facet_var,
                                  color_manual,
                                  shape_manual) {

  ns <- session$ns

  # filter seected biomarkers
  anl_param <- reactive({
    validate(need(input$xaxis_param, "Please select a biomarker"))

    dataset_var <- paste0(dataname, "_FILTERED")
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
    validate_has_data(ANL_FILTERED, 5)


    validate_has_variable(ANL_FILTERED, param_var)

    validate_in(
      input$xaxis_param, unique(ANL_FILTERED[[param_var]]),
      sprintf("X-Axis Biomarker %s is not available in data %s", input$xaxis_param, dataname))

    validate_in(
      input$yaxis_param, unique(ANL_FILTERED[[param_var]]),
      sprintf("Y-Axis Biomarker %s is not available in data %s", input$yaxis_param, dataname))

    validate_has_variable(
      ANL_FILTERED,
      "AVISITCD",
      sprintf("Variable AVISITCD is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      "BASE",
      sprintf("Variable BASE is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      "BASE2",
      sprintf("Variable BASE2 is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      facet_var,
      sprintf("Variable %s is not available in data %s", facet_var, dataname))

    validate_has_variable(
      ANL_FILTERED,
      "LOQFL",
      sprintf("Variable LOQFL is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      "PARAM",
      sprintf("Variable PARAM is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      "LBSTRESC",
      sprintf("Variable LBSTRESC is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      trt_group,
      sprintf("Variable %s is not available in data %s", trt_group, dataname))

    validate_has_variable(
      ANL_FILTERED,
      "USUBJID",
      sprintf("Variable USUBJID is not available in data %s", dataname))

    validate_has_variable(
      ANL_FILTERED,
      input$xaxis_var,
      sprintf("Variable %s is not available in data %s", input$xaxis_var, dataname))

    validate_has_variable(
      ANL_FILTERED,
      input$yaxis_var,
      sprintf("Variable %s is not available in data %s", input$yaxis_var, dataname))

    # analysis
    private_chunks <- chunks$new()
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)), private_chunks)

    # filter biomarker
    chunks_push(
      chunks = private_chunks,
      id = "filter_biomarker",
      expression = bquote({
        ANL <- .(as.name(dataset_var)) %>% # nolint
          dplyr::filter(.data[[.(param_var)]] %in% union(.(input$xaxis_param), .(input$yaxis_param)))
      })
    )

    ANL <- chunks_safe_eval(private_chunks) # nolint
    validate_has_data(ANL, 5)

    return(list(ANL = ANL, chunks = private_chunks))
  })

  # constraints
  observe({
    constraint_var <- input$constraint_var
    validate(need(constraint_var, "select a constraint variable"))

    # note that filtered is false thus we cannot use anl_param()$ANL
    ANL <- datasets$get_data(dataname, filtered = FALSE) # nolint

    validate_has_variable(ANL, param_var)
    validate_has_variable(ANL, "AVISITCD")
    validate_has_variable(ANL, "BASE")
    validate_has_variable(ANL, "BASE2")

    ANL <- ANL %>% filter(.data[[param_var]] == input$xaxis_param) # nolint

    visit_freq <- unique(ANL$AVISITCD)

    # get min max values
    if ((constraint_var == "BASE2" && any(grepl("SCR", visit_freq))) ||
        (constraint_var == "BASE" && any(grepl("BL", visit_freq)))) {

      val <- na.omit(switch(
        constraint_var,
        "BASE" = ANL$BASE[ANL$AVISITCD == "BL"],
        "BASE2" = ANL$BASE2[ANL$AVISITCD == "SCR"],
        stop(paste(constraint_var, "not allowed"))
      ))

      if (length(val) == 0 || all(is.na(val))) {
        shinyjs::show("all_na")
        shinyjs::hide("constraint_range")
        args <- list(
          min = list(label = "Min", min = 0, max = 0, value = 0),
          max = list(label = "Max", min = 0, max = 0, value = 0)
        )
        update_min_max(session, args)
      } else {
        rng <- range(val, na.rm = TRUE)

        minmax <- c(floor(rng[1] * 1000) / 1000,  ceiling(rng[2] * 1000) / 1000)

        label_min <- sprintf("Min (%s)", minmax[1])
        label_max <- sprintf("Max (%s)", minmax[2])

        args <- list(
          min = list(label = label_min, min = minmax[1], max = minmax[2], value = minmax[1]),
          max = list(label = label_max, min = minmax[1], max = minmax[2], value = minmax[2])
        )

        update_min_max(session, args)
        shinyjs::show("constraint_range") # update before show
        shinyjs::hide("all_na")
      }
    } else if (constraint_var == "NONE") {

      shinyjs::hide("constraint_range") # hide before update
      shinyjs::hide("all_na")

      # force update (and thus refresh) on different constraint_var -> pass unique value for each constraint_var name
      args <- list(
        min = list(label = "Min", min = 0, max = 0, value = 0),
        max = list(label = "Max", min = 0, max = 0, value = 0)
      )

      update_min_max(session, args)
    } else {
      stop("invalid contraint_var", constraint_var)
    }
  })

  anl_constraint <- create_anl_constraint_reactive(anl_param, input, param_id = "xaxis_param")

  # update sliders for axes taking constraints into account
  xrange_slider <- callModule(toggle_slider_server, "xrange_scale")
  yrange_slider <- callModule(toggle_slider_server, "yrange_scale")
  keep_range_slider_updated(session, input, xrange_slider$update_state, "xaxis_var", "xaxis_param", anl_constraint)
  keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "yaxis_param", anl_constraint)
  keep_data_const_opts_updated(session, input, anl_constraint, "xaxis_param")

  # selector names after transposition
  xvar <- reactive(paste0(input$xaxis_var, ".", input$xaxis_param))
  yvar <- reactive(paste0(input$yaxis_var, ".", input$yaxis_param))
  xloqfl <- reactive(paste0("LOQFL_", input$xaxis_param))
  yloqfl <- reactive(paste0("LOQFL_", input$yaxis_param))

  # transpose data to plot
  plot_data_transpose <- reactive({
    private_chunks <- anl_constraint()$chunks$clone(deep = TRUE)
    ANL <- anl_constraint()$ANL # nolint
    chunks_push(
      chunks = private_chunks,
      id = "plot_data_transpose",
      expression = bquote({

        ANL_TRANSPOSED1 <- ANL %>% # nolint
          dplyr::select(
            .data[["USUBJID"]],
            .data[[.(trt_group)]],
            .data[["AVISITCD"]],
            .data[[.(param_var)]],
            .data[[.(input$xaxis_var)]],
            .data[[.(input$yaxis_var)]]) %>%
          tidyr::gather(
            key = "ANLVARS",
            value = "ANLVALS",
            .data[[.(input$xaxis_var)]],
            .data[[.(input$yaxis_var)]]) %>%
          tidyr::unite(
            "ANL.PARAM",
            "ANLVARS",
            .(param_var),
            sep = ".",
            remove = TRUE) %>%
          tidyr::spread(.data[["ANL.PARAM"]], .data[["ANLVALS"]]) %>%
          dplyr::filter(!is.na(.data[[.(xvar())]]) & !is.na(.data[[.(yvar())]]))

        ANL_TRANSPOSED2 <- ANL %>% # nolint
          dplyr::select(
            .data[["USUBJID"]],
            .data[[.(trt_group)]],
            .data[["AVISITCD"]],
            .data[[.(param_var)]],
            .data[["LOQFL"]],
            .data[["PARAM"]],
            .data[["LBSTRESC"]]) %>%
          tidyr::gather(
            key = "ANLVARS",
            value = "ANLVALS",
            .data[["LOQFL"]],
            .data[["PARAM"]],
            .data[["LBSTRESC"]]) %>%
          tidyr::unite(
            "ANL.PARAM",
            "ANLVARS",
            .(param_var),
            sep = "_",
            remove = TRUE) %>%
          tidyr::spread(.data[["ANL.PARAM"]], .data[["ANLVALS"]]) %>%
          dplyr::mutate(LOQFL_COMB = case_when(
            .data[[.(xloqfl())]] == "Y" | .data[[.(yloqfl())]] == "Y" ~ "Y",
            .data[[.(xloqfl())]] == "N" & .data[[.(yloqfl())]] == "N" ~ "N",
            .data[[.(xloqfl())]] == "N" & .data[[.(yloqfl())]] == "NA" ~ "N",
            .data[[.(xloqfl())]] == "NA" & .data[[.(yloqfl())]] == "N" ~ "N",
            .data[[.(xloqfl())]] == "NA" & .data[[.(yloqfl())]] == "NA" ~ "NA",
            TRUE ~ as.character(NA))
          )

        ANL_TRANSPOSED <- merge(ANL_TRANSPOSED1, ANL_TRANSPOSED2) # nolint

      })
    )

    ANL_TRANSPOSED <- chunks_safe_eval(private_chunks) # nolint
    chunks_push_new_line(private_chunks)

    validate(need(nrow(ANL_TRANSPOSED) > 0, "Plot Data No Observations Left"))
    validate_has_variable(data = ANL_TRANSPOSED, varname = c(xvar(), yvar(), xloqfl(), yloqfl()))


    chunks_push(
      chunks = private_chunks,
      id = "ANL_attributes",
      expression = if (trt_group == "ARM") {
        bquote(attr(ANL_TRANSPOSED$ARM, "label") <- "Planned Arm")
      } else {
        bquote(attr(ANL_TRANSPOSED[[.(trt_group)]], "label") <- "Actual Arm")
      }
    )
    chunks_push_new_line(private_chunks)


    return(list(ANL_TRANSPOSED = ANL_TRANSPOSED, chunks = private_chunks))
  })

  plot_labels <- reactive({
    ANL <- chunks_get_var(var = "ANL", anl_constraint()$chunks) # nolint

    xparam <- ANL$PARAM[ANL[[param_var]] == input$xaxis_param][1]
    yparam <- ANL$PARAM[ANL[[param_var]] == input$yaxis_param][1]

    # setup the x-axis label.  Combine the biomarker and the units (if available)
    if (is.null(ANL$AVALU) || all(ANL[["AVALU"]] == "")) {
      title_text <- paste(xparam, "and", yparam, "@ Visits")
      xaxis_lab <- paste(xparam, input$xaxis_var, "Values")
      yaxis_lab <- paste(yparam, input$yaxis_var, "Values")

    } else {
      xunit <- ANL$AVALU[ANL[[param_var]] == input$xaxis_param][1]
      yunit <- ANL$AVALU[ANL[[param_var]] == input$yaxis_param][1]

      title_text <- paste0(xparam, " (", xunit, ") and ", yparam, " (", yunit, ") @ Visits")
      xaxis_lab <- paste0(xparam, " (", xunit, ") ", input$xaxis_var, " Values")
      yaxis_lab <- paste0(yparam, " (", yunit, ") ", input$yaxis_var, " Values")
    }

    list(title_text = title_text, xaxis_lab = xaxis_lab, yaxis_lab = yaxis_lab)
  })

  # plot
  output$plot <- renderPlot({
    private_chunks <- plot_data_transpose()$chunks$clone(deep = TRUE)
    # nolint start
    xaxis_param <- input$xaxis_param
    xaxis_var <- input$xaxis_var
    yaxis_param <- input$yaxis_param
    yaxis_var <- input$yaxis_var
    xmin_scale <- xrange_slider$state()$value[[1]]
    xmax_scale <- xrange_slider$state()$value[[2]]
    ymin_scale <- yrange_slider$state()$value[[1]]
    ymax_scale <- yrange_slider$state()$value[[2]]
    font_size <- input$font_size
    dot_size <- input$dot_size
    reg_text_size <- input$reg_text_size
    hline <- if (is.na(input$hline)) NULL else as.numeric(input$hline)
    vline <- if (is.na(input$vline)) NULL else as.numeric(input$vline)
    facet_ncol <- input$facet_ncol
    visit_facet <- input$visit_facet
    facet <- input$facet
    reg_line <- input$reg_line
    loq_legend <- input$loq_legend
    rotate_xlab <- input$rotate_xlab
    # nolint end
    title_text <- plot_labels()$title_text
    xaxis_lab  <- plot_labels()$xaxis_lab
    yaxis_lab  <- plot_labels()$yaxis_lab

    chunks_push(
      chunks = private_chunks,
      id = "scatterplot",
      expression = bquote({
        # re-establish treatment variable label
        goshawk::g_correlationplot(
          data = ANL_TRANSPOSED,
          param_var = .(param_var),
          xaxis_param = .(xaxis_param),
          xaxis_var = .(xaxis_var),
          xvar = .(xvar()),
          yaxis_param = .(yaxis_param),
          yaxis_var = .(yaxis_var),
          yvar = .(yvar()),
          trt_group = .(trt_group),
          xmin = .(xmin_scale),
          xmax = .(xmax_scale),
          ymin = .(ymin_scale),
          ymax = .(ymax_scale),
          title_text = .(title_text),
          xaxis_lab = .(xaxis_lab),
          yaxis_lab = .(yaxis_lab),
          color_manual = .(color_manual),
          shape_manual = .(shape_manual),
          facet_ncol = .(facet_ncol),
          visit_facet = .(visit_facet),
          facet = .(facet),
          facet_var = .(facet_var),
          reg_line = .(reg_line),
          font_size = .(font_size),
          dot_size = .(dot_size),
          reg_text_size = .(reg_text_size),
          loq_legend = .(loq_legend),
          rotate_xlab = .(rotate_xlab),
          hline = .(hline),
          vline = .(vline)
        )
      })
    )

    p <- chunks_safe_eval(private_chunks)

    # promote chunks to be visible in the sessionData by other modules
    init_chunks(private_chunks)

    p

  })

  # dynamic plot height and brushing
  output$plot_ui <- renderUI({

    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))

    plotOutput(
      ns("plot"),
      height = plot_height,
      brush = brushOpts(id = ns("plot_brush"), resetOnNew = TRUE)
    )
  })

  # highlight plot area
  output$brush_data <- DT::renderDataTable({
    req(input$plot_brush)
    ANL_TRANSPOSED <- isolate(plot_data_transpose()$ANL_TRANSPOSED) # nolint

    df <- brushedPoints(
      select(ANL_TRANSPOSED, "USUBJID", trt_group, "AVISITCD", xvar(), yvar(), "LOQFL_COMB"),
      input$plot_brush
    )

    numeric_cols <- names(select_if(df, is.numeric))

    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(numeric_cols, 4)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Correlation Plot"
  )
}
