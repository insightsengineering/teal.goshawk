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
#' @param yaxis_var name of variable containing biomarker results displayed on y-axis e.g. AVAL. When not provided,
#' it defaults to `choices_selected(c("AVAL", "CHG"), "AVAL")`.
#' @param xaxis_var variable to categorize the x-axis. When not provided, it defaults to
#' `choices_selected("AVISITCD", "AVISITCD")`.
#' @param facet_var variable to facet the plots by. When not provided, it defaults to
#' `choices_selected(c("ARM", "ACTARM"), "ARM")`.
#' @param trt_group  \code{\link[teal]{choices_selected}} object with available choices and pre-selected option
#'  for variable names representing treatment group e.g. ARM.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param loq_legend loq legend toggle.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param hline_vars a character vector to name the columns that will define additional horizontal lines.
#' @param hline_vars_colors a character vector naming the colors for the additional horizontal lines.
#' @param hline_vars_labels a character vector naming the labels for the additional horizontal lines that will appear
#'  in the legend.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
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
#' @import teal.devel
#'
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#' @author Balazs Toth (tothb2) toth.balazs@gene.com
#'
#' @return an \code{\link[teal]{module}} object
#'
#' @export
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(scda)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#'
#' set.seed(1)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   mutate(
#'     AVISITCD = case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)
#'     ),
#'     AVISITCDN = case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)
#'     ),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3
#'     ),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD),
#'     ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'     ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'     ANRLO = 50,
#'     ANRHI = 75
#'   ) %>%
#'   rowwise() %>%
#'   group_by(PARAMCD) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'   )) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'   )) %>%
#'   ungroup()
#'
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
#' attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#' attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#' # add LLOQ and ULOQ variables
#' ALB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#' ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     adsl <- cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "
#'         set.seed(1)
#'         ADLB <- synthetic_cdisc_data('latest')$adlb
#'         var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'         ADLB <- ADLB %>%
#'           mutate(AVISITCD = case_when(
#'             AVISIT == 'SCREENING' ~ 'SCR',
#'             AVISIT == 'BASELINE' ~ 'BL',
#'             grepl('WEEK', AVISIT) ~ paste('W', stringr::str_extract(AVISIT, '(?<=(WEEK ))[0-9]+')),
#'             TRUE ~ as.character(NA)),
#'             AVISITCDN = case_when(
#'               AVISITCD == 'SCR' ~ -2,
#'               AVISITCD == 'BL' ~ 0,
#'               grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'               TRUE ~ as.numeric(NA)),
#'             AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'             TRTORD = case_when(
#'               ARMCD == 'ARM C' ~ 1,
#'               ARMCD == 'ARM B' ~ 2,
#'               ARMCD == 'ARM A' ~ 3),
#'             ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'             ARM = factor(ARM) %>% reorder(TRTORD),
#'             ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'             ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'             ANRLO = 50,
#'             ANRHI = 75) %>%
#'           rowwise() %>%
#'           group_by(PARAMCD) %>%
#'           mutate(LBSTRESC = ifelse(
#'             USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'             paste('<', round(runif(1, min = 25, max = 30))), LBSTRESC)) %>%
#'           mutate(LBSTRESC = ifelse(
#'             USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'             paste( '>', round(runif(1, min = 70, max = 75))), LBSTRESC)) %>%
#'           ungroup()
#'         attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'         attr(ADLB[['ACTARM']], 'label') <- var_labels[['ACTARM']]
#'         attr(ADLB[['ANRLO']], 'label') <- 'Analysis Normal Range Lower Limit'
#'         attr(ADLB[['ANRHI']], 'label') <- 'Analysis Normal Range Upper Limit'
#'         # add LLOQ and ULOQ variables
#'         ALB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#'         ADLB <- left_join(ADLB, ALB_LOQS, by = 'PARAM')",
#'       vars = list(ADSL = adsl, arm_mapping = arm_mapping)
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_gh_boxplot(
#'       label = "Box Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG"), "AVAL"),
#'       xaxis_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "STUDYID"), "ARM"),
#'       facet_var = choices_selected(c("ACTARM", "ARM", "AVISITCD", "SEX"), "AVISITCD"),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       loq_legend = TRUE,
#'       rotate_xlab = FALSE,
#'       hline_arb = c(60, 55),
#'       hline_arb_color = c("grey", "red"),
#'       hline_arb_label = c("default_hori_A", "default_hori_B"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("pink", "brown", "purple", "black"),
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_boxplot <- function(label,
                            dataname,
                            param_var,
                            param,
                            yaxis_var = choices_selected(c("AVAL", "CHG"), "AVAL"),
                            xaxis_var = choices_selected("AVISITCD", "AVISITCD"),
                            facet_var = choices_selected(c("ARM", "ACTARM"), "ARM"),
                            trt_group,
                            color_manual = NULL,
                            shape_manual = NULL,
                            facet_ncol = NULL,
                            loq_legend = TRUE,
                            rotate_xlab = FALSE,
                            hline_arb = numeric(0),
                            hline_arb_color = "red",
                            hline_arb_label = "Horizontal line",
                            hline_vars = character(0),
                            hline_vars_colors = "green",
                            hline_vars_labels = hline_vars,
                            plot_height = c(600, 200, 2000),
                            plot_width = NULL,
                            font_size = c(12, 8, 20),
                            dot_size = c(2, 1, 12),
                            alpha = c(0.8, 0.0, 1.0),
                            pre_output = NULL,
                            post_output = NULL) {
  logger::log_info("Initializing tm_g_gh_boxplot")
  checkmate::assert_string(label)
  checkmate::assert_string(dataname)
  checkmate::assert_string(param_var)
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(facet_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  checkmate::assert_int(facet_ncol, null.ok = TRUE)
  checkmate::assert_flag(loq_legend)
  checkmate::assert_flag(rotate_xlab)
  checkmate::assert_numeric(font_size, len = 3)
  checkmate::assert_numeric(dot_size, len = 3)
  checkmate::assert_numeric(alpha, len = 3)
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  args <- as.list(environment())

  module(
    label = label,
    filters = dataname,
    server = srv_g_boxplot,
    server_args = list(
      dataname = dataname,
      param_var = param_var,
      trt_group = trt_group,
      facet_var = facet_var,
      color_manual = color_manual,
      shape_manual = shape_manual,
      plot_height = plot_height,
      plot_width = plot_width,
      hline_vars_colors = hline_vars_colors,
      hline_vars_labels = hline_vars_labels
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
        plot_with_settings_ui(id = ns("boxplot"))
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
        DT::dataTableOutput(ns("table_ui"))
      ))
    ),
    encoding = div(
      templ_ui_dataname(a$dataname),
      optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = a$trt_group$choices,
        selected = a$trt_group$selected,
        multiple = FALSE
      ),
      templ_ui_params_vars(
        ns,
        xparam_choices = a$param$choices,
        xparam_selected = a$param$selected,
        xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices,
        xselected = a$xaxis_var$selected,
        ychoices = a$yaxis_var$choices,
        yselected = a$yaxis_var$selected
      ),
      optionalSelectInput(
        ns("facet_var"),
        label = "Facet by",
        choices = a$facet_var$choices,
        selected = a$facet_var$selected,
        multiple = FALSE
      ),
      templ_ui_constraint(ns, label = "Data Constraint"), # required by constr_anl_chunks
      if (length(a$hline_vars) > 0) {
        optionalSelectInput(
          ns("hline_vars"),
          label = "Add Horizontal Range Line(s):",
          choices = a$hline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(id = ns("hline_arb"), a$hline_arb, a$hline_arb_label, a$hline_arb_color),
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          toggle_slider_ui(
            ns("yrange_scale"),
            label = "Y-Axis Range Zoom",
            min = -1000000,
            max = 1000000,
            value = c(-1000000, 1000000)
          ),
          numericInput(ns("facet_ncol"), "Number of Plots Per Row:", a$facet_ncol, min = 1),
          checkboxInput(ns("loq_legend"), "Display LoQ Legend", a$loq_legend),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dot_size"), "Dot Size", a$dot_size, ticks = FALSE),
          optionalSliderInputValMinMax(ns("alpha"), "Dot Alpha", a$alpha, ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_g_boxplot <- function(id,
                          datasets,
                          dataname,
                          param_var,
                          trt_group,
                          facet_var,
                          color_manual,
                          shape_manual,
                          plot_height,
                          plot_width,
                          hline_vars_colors,
                          hline_vars_labels) {
  moduleServer(id, function(input, output, session) {
    init_chunks()

    # reused in all modules
    anl_chunks <- constr_anl_chunks(
      session, input, datasets, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 2
    )
    # update sliders for axes taking constraints into account
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(
      session,
      input,
      update_slider_fcn = yrange_slider$update_state,
      id_var = "yaxis_var",
      id_param_var = "xaxis_param",
      reactive_ANL = anl_chunks
    )
    keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    create_plot <- reactive({
      private_chunks <- anl_chunks()$chunks$clone(deep = TRUE)
      # nolint start
      param <- input$xaxis_param
      yaxis <- input$yaxis_var
      xaxis <- input$xaxis_var
      facet_var <- `if`(is.null(input$facet_var), "None", input$facet_var)
      yrange_scale <- yrange_slider$state()$value
      facet_ncol <- input$facet_ncol
      validate(need(
        is.na(facet_ncol) || (as.numeric(facet_ncol) > 0 && as.numeric(facet_ncol) %% 1 == 0),
        "Number of plots per row must be a positive integer"
      ))
      alpha <- input$alpha
      font_size <- input$font_size
      dot_size <- input$dot_size
      loq_legend <- input$loq_legend
      rotate_xlab <- input$rotate_xlab

      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color

      hline_vars <- input$hline_vars
      trt_group <- input$trt_group
      # nolint end
      validate(need(input$trt_group, "Please select a treatment variable"))
      validate(need(!is.null(xaxis), "Please select an X-Axis Variable"))
      validate(need(!is.null(yaxis), "Please select a Y-Axis Variable"))
      validate_has_variable(
        anl_chunks()$ANL,
        yaxis,
        sprintf("Variable %s is not available in data %s", yaxis, dataname)
      )
      validate_has_variable(
        anl_chunks()$ANL,
        xaxis,
        sprintf("Variable %s is not available in data %s", xaxis, dataname)
      )

      if (!facet_var == "None") {
        validate_has_variable(
          anl_chunks()$ANL,
          facet_var,
          sprintf("Variable %s is not available in data %s", facet_var, dataname)
        )
      }

      validate(need(
        !facet_var %in% c("ACTARM", "ARM")[!c("ACTARM", "ARM") %in% trt_group],
        sprintf("You can not choose %s as facetting variable for treatment variable %s.", facet_var, trt_group)
      ))
      validate(need(
        !xaxis %in% c("ACTARM", "ARM")[!c("ACTARM", "ARM") %in% trt_group],
        sprintf("You can not choose %s as x-axis variable for treatment variable %s.", xaxis, trt_group)
      ))

      chunks_push(
        chunks = private_chunks,
        id = "boxplot",
        expression = bquote({
          p <- goshawk::g_boxplot(
            data = ANL,
            biomarker = .(param),
            xaxis_var = .(xaxis),
            yaxis_var = .(yaxis),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars)]),
            facet_ncol = .(facet_ncol),
            loq_legend = .(loq_legend),
            rotate_xlab = .(rotate_xlab),
            trt_group = .(trt_group),
            ylim = .(yrange_scale),
            color_manual = .(color_manual),
            shape_manual = .(shape_manual),
            facet_var = .(facet_var),
            alpha = .(alpha),
            dot_size = .(dot_size),
            font_size = .(font_size),
            unit = .("AVALU")
          )
        })
      )

      chunks_safe_eval(private_chunks)

      private_chunks
    })

    create_table <- reactive({
      private_chunks <- create_plot()$clone(deep = TRUE)

      param <- input$xaxis_param
      xaxis_var <- input$yaxis_var # nolint
      font_size <- input$font_size
      trt_group <- input$trt_group

      chunks_push(
        chunks = private_chunks,
        id = "table",
        expression = bquote({
          tbl <- goshawk::t_summarytable(
            data = ANL,
            trt_group = .(trt_group),
            param_var = .(param_var),
            param = .(param),
            xaxis_var = .(xaxis_var),
            facet_var = .(input$facet_var)
          )
        })
      )

      chunks_safe_eval(private_chunks)
      private_chunks
    })

    main_code <- reactive({
      private_chunks <- create_table()
      chunks_push(
        chunks = private_chunks,
        id = "output",
        expression = quote(print(p))
      )

      chunks_safe_eval(private_chunks)

      chunks_reset()
      chunks_push_chunks(private_chunks)

      private_chunks
    })

    plot_r <- reactive({
      chunks_get_var("p", main_code())
    })

    boxplot_data <- plot_with_settings_srv(
      id = "boxplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    output$table_ui <- DT::renderDataTable({
      tbl <- chunks_get_var("tbl", main_code())

      numeric_cols <- setdiff(names(select_if(tbl, is.numeric)), "n")

      DT::datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    # highlight plot area
    output$brush_data <- DT::renderDataTable({
      boxplot_brush <- boxplot_data$brush()

      ANL <- isolate(anl_chunks()$ANL) %>% droplevels() # nolint
      validate_has_data(ANL, 2)

      xvar <- isolate(input$xaxis_var)
      yvar <- isolate(input$yaxis_var)
      facetv <- isolate(input$facet_var)
      trt_group <- isolate(input$trt_group)

      req(all(c(xvar, yvar, facetv, trt_group) %in% names(ANL)))

      df <- clean_brushedPoints(
        select(ANL, "USUBJID", trt_group, facetv, "AVISITCD", "PARAMCD", xvar, yvar, "LOQFL"),
        boxplot_brush
      )

      numeric_cols <- names(select_if(df, is.numeric))

      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = "Box Plot"
    )
  })
}
