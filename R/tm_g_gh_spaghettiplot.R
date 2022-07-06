#' Spaghetti Plot
#'
#' This teal module renders the UI and calls the function
#' that creates a spaghetti plot.
#'
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init.
#' E.g. ADaM structured laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data
#' that includes parameter labels.
#' @param idvar name of unique subject id variable.
#' @param xaxis_var single name of variable in analysis data
#' that is used as x-axis in the plot for the respective goshawk function.
#' @param xaxis_var_level vector that can be used to define the factor level of xaxis_var.
#' Only use it when xaxis_var is character or factor.
#' @param filter_var data constraint variable.
#' @param yaxis_var single name of variable in analysis data that is used as
#' summary variable in the respective gshawk function.
#' @param trt_group \code{\link[teal.transform]{choices_selected}} object with available choices and pre-selected option
#' for variable names representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor
#' level of trt_group.
#' @param man_color string vector representing customized colors
#' @param color_comb name or hex value for combined treatment color.
#' @param xtick numeric vector to define the tick values of x-axis
#' when x variable is numeric. Default value is waive().
#' @param xlabel vector with same length of xtick to define the
#' label of x-axis tick values. Default value is waive().
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels
#' @param facet_ncol numeric value indicating number of facets per row.
#' @param plot_height controls plot height.
#' @param plot_width optional, controls plot width.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param group_stats control group mean or median overlay.
#' @param hline_arb numeric vector of at most 2 values identifying intercepts for arbitrary horizontal lines.
#' @param hline_arb_color a character vector of at most length of \code{hline_arb}.
#' naming the color for the arbitrary horizontal lines.
#' @param hline_arb_label a character vector of at most length of \code{hline_arb}.
#' naming the label for the arbitrary horizontal lines.
#' @param hline_vars a character vector to name the columns that will define additional horizontal lines.
#' @param hline_vars_colors a character vector naming the colors for the additional horizontal lines.
#' @param hline_vars_labels a character vector naming the labels for the additional horizontal lines that will appear
#'  in the legend.
#' @inheritParams teal.widgets::standard_layout
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
#' set.seed(1)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   dplyr::mutate(
#'     AVISITCD = dplyr::case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
#'       TRUE ~ as.character(NA)
#'     ),
#'     AVISITCDN = dplyr::case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ as.numeric(NA)
#'     ),
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = dplyr::case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3
#'     ),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD),
#'     ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'     ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'     ANRLO = 30,
#'     ANRHI = 75
#'   ) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::group_by(PARAMCD) %>%
#'   dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'   )) %>%
#'   dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'   )) %>%
#'   ungroup()
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
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "set.seed(1)
#'               ADLB <- synthetic_cdisc_data(\"latest\")$adlb
#'               var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'               ADLB <- ADLB %>%
#'                 dplyr::mutate(AVISITCD = dplyr::case_when(
#'                     AVISIT == 'SCREENING' ~ 'SCR',
#'                     AVISIT == 'BASELINE' ~ 'BL',
#'                     grepl('WEEK', AVISIT) ~
#'                       paste('W', stringr::str_extract(AVISIT, '(?<=(WEEK ))[0-9]+')),
#'                     TRUE ~ as.character(NA)),
#'                   AVISITCDN = dplyr::case_when(
#'                     AVISITCD == 'SCR' ~ -2,
#'                     AVISITCD == 'BL' ~ 0,
#'                     grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'                     TRUE ~ as.numeric(NA)),
#'                   AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'                   TRTORD = dplyr::case_when(
#'                     ARMCD == 'ARM C' ~ 1,
#'                     ARMCD == 'ARM B' ~ 2,
#'                     ARMCD == 'ARM A' ~ 3),
#'                   ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'                   ARM = factor(ARM) %>% reorder(TRTORD),
#'                   ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
#'                   ACTARM = factor(ACTARM) %>% reorder(TRTORD),
#'                   ANRLO = 30,
#'                   ANRHI = 75) %>%
#'                   dplyr::rowwise() %>%
#'                   dplyr::group_by(PARAMCD) %>%
#'                   dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'                   paste('<', round(runif(1, min = 25, max = 30))), LBSTRESC)) %>%
#'                   dplyr::mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'                   paste( '>', round(runif(1, min = 70, max = 75))), LBSTRESC)) %>%
#'                   ungroup
#'                attr(ADLB[['ARM']], 'label') <- var_labels[['ARM']]
#'                attr(ADLB[['ACTARM']], 'label') <- var_labels[['ACTARM']]
#'                attr(ADLB[['ANRLO']], 'label') <- 'Analysis Normal Range Lower Limit'
#'                attr(ADLB[['ANRHI']], 'label') <- 'Analysis Normal Range Upper Limit'
#'                ALB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#'                ADLB <- left_join(ADLB, ALB_LOQS, by = 'PARAM')",
#'       vars = list(arm_mapping = arm_mapping)
#'     ),
#'     check = FALSE
#'   ),
#'   modules = modules(
#'     tm_g_gh_spaghettiplot(
#'       label = "Spaghetti Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       idvar = "USUBJID",
#'       xaxis_var = choices_selected(c("Analysis Visit Code" = "AVISITCD"), "AVISITCD"),
#'       yaxis_var = choices_selected(c("AVAL", "CHG", "PCHG"), "AVAL"),
#'       filter_var = choices_selected(
#'         c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE"),
#'         "NONE"
#'       ),
#'       trt_group = choices_selected(c("ARM", "ACTARM"), "ARM"),
#'       color_comb = "#39ff14",
#'       man_color = c(
#'         "Combination" = "#000000",
#'         "Placebo" = "#fce300",
#'         "150mg QD" = "#5a2f5f"
#'       ),
#'       hline_arb = c(60, 50),
#'       hline_arb_color = c("grey", "red"),
#'       hline_arb_label = c("default A", "default B"),
#'       hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'       hline_vars_colors = c("pink", "brown", "purple", "black"),
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_gh_spaghettiplot <- function(label,
                                  dataname,
                                  param_var,
                                  param,
                                  param_var_label = "PARAM",
                                  idvar,
                                  xaxis_var,
                                  yaxis_var,
                                  xaxis_var_level = NULL,
                                  filter_var = yaxis_var,
                                  trt_group,
                                  trt_group_level = NULL,
                                  group_stats = "NONE",
                                  man_color = NULL,
                                  color_comb = NULL,
                                  xtick = ggplot2::waiver(),
                                  xlabel = xtick,
                                  rotate_xlab = FALSE,
                                  facet_ncol = 2,
                                  plot_height = c(600, 200, 2000),
                                  plot_width = NULL,
                                  font_size = c(12, 8, 20),
                                  hline_arb = numeric(0),
                                  hline_arb_color = "red",
                                  hline_arb_label = "Horizontal line",
                                  hline_vars = character(0),
                                  hline_vars_colors = "green",
                                  hline_vars_labels = hline_vars,
                                  pre_output = NULL,
                                  post_output = NULL) {
  logger::log_info("Initializing tm_g_gh_spaghettiplot")
  checkmate::assert_class(param, "choices_selected")
  checkmate::assert_class(xaxis_var, "choices_selected")
  checkmate::assert_class(yaxis_var, "choices_selected")
  checkmate::assert_class(trt_group, "choices_selected")
  validate_line_arb_arg(hline_arb, hline_arb_color, hline_arb_label)
  validate_line_vars_arg(hline_vars, hline_vars_colors, hline_vars_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
    .var.name = "plot_width"
  )

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_spaghettiplot,
    server_args = list(
      dataname = dataname,
      idvar = idvar,
      param_var = param_var,
      trt_group = trt_group,
      xaxis_var_level = xaxis_var_level,
      trt_group_level = trt_group_level,
      man_color = man_color,
      color_comb = color_comb,
      param_var_label = param_var_label,
      xtick = xtick,
      xlabel = xlabel,
      plot_height = plot_height,
      plot_width = plot_width,
      hline_vars_colors = hline_vars_colors,
      hline_vars_labels = hline_vars_labels
    ),
    ui = g_ui_spaghettiplot,
    ui_args = args,
    filters = dataname
  )
}

g_ui_spaghettiplot <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  teal.widgets::standard_layout(
    output = templ_ui_output_datatable(ns),
    encoding = div(
      ### Reporter
      shiny::tags$div(
        teal.reporter::add_card_button_ui(ns("addReportCard")),
        teal.reporter::download_report_button_ui(ns("downloadButton")),
        teal.reporter::reset_report_button_ui(ns("resetButton"))
      ),
      shiny::tags$br(),
      ###
      templ_ui_dataname(a$dataname),
      teal.widgets::optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = a$trt_group$choices,
        selected = a$trt_group$selected,
        multiple = FALSE
      ),
      templ_ui_params_vars(
        ns,
        # xparam and yparam are identical, so we only show the user one
        xparam_choices = a$param$choices, xparam_selected = a$param$selected, xparam_label = "Select a Biomarker",
        xchoices = a$xaxis_var$choices, xselected = a$xaxis_var$selected,
        ychoices = a$yaxis_var$choices, yselected = a$yaxis_var$selected
      ),
      radioButtons(
        ns("group_stats"),
        "Group Statistics",
        c("None" = "NONE", "Mean" = "MEAN", "Median" = "MEDIAN"),
        inline = TRUE
      ),
      templ_ui_constraint(ns), # required by constr_anl_chunks
      if (length(a$hline_vars) > 0) {
        teal.widgets::optionalSelectInput(
          ns("hline_vars"),
          label = "Add Horizontal Range Line(s):",
          choices = a$hline_vars,
          selected = NULL,
          multiple = TRUE
        )
      },
      ui_arbitrary_lines(id = ns("hline_arb"), a$hline_arb, a$hline_arb_label, a$hline_arb_color),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot Aesthetic Settings",
          div(
            style = "padding: 0px;",
            toggle_slider_ui(
              ns("yrange_scale"),
              label = "Y-Axis Range Zoom",
              min = -1000000,
              max = 1000000,
              value = c(-1000000, 1000000)
            ),
            div(
              style = "display: inline-block;vertical-align:middle; width: 175px;",
              tags$b("Number of Plots Per Row:")
            ),
            div(
              style = "display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("facet_ncol"), "", a$facet_ncol, min = 1)
            )
          ),
          checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
          teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            ns("alpha"),
            "Line Alpha",
            a$alpha,
            value_min_max = c(0.8, 0.0, 1.0), step = 0.1, ticks = FALSE
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}



srv_g_spaghettiplot <- function(id,
                                datasets,
                                reporter,
                                dataname,
                                idvar,
                                param_var,
                                trt_group,
                                man_color,
                                color_comb,
                                xaxis_var_level,
                                trt_group_level,
                                param_var_label,
                                xtick,
                                xlabel,
                                plot_height,
                                plot_width,
                                hline_vars_colors,
                                hline_vars_labels) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    # reused in all modules
    anl_chunks <- constr_anl_chunks(
      session, input, datasets, dataname,
      param_id = "xaxis_param", param_var = param_var, trt_group = input$trt_group, min_rows = 1
    )

    # update sliders for axes taking constraints into account
    yrange_slider <- toggle_slider_server("yrange_scale")
    keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "xaxis_param", anl_chunks)
    keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

    horizontal_line <- srv_arbitrary_lines("hline_arb")

    plot_r <- reactive({
      # nolint start
      private_chunks <- teal.code::chunks_deep_clone(anl_chunks()$chunks)
      ylim <- yrange_slider$state()$value
      facet_ncol <- input$facet_ncol
      validate(need(
        is.na(facet_ncol) || (as.numeric(facet_ncol) > 0 && as.numeric(facet_ncol) %% 1 == 0),
        "Number of plots per row must be a positive integer"
      ))
      rotate_xlab <- input$rotate_xlab
      hline_arb <- horizontal_line()$line_arb
      hline_arb_label <- horizontal_line()$line_arb_label
      hline_arb_color <- horizontal_line()$line_arb_color
      group_stats <- input$group_stats
      font_size <- input$font_size
      alpha <- input$alpha
      validate(need(input$trt_group, "Please select a treatment variable"))
      trt_group <- input$trt_group

      # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_chunk()) and some inputs
      validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
      validate(need(input$yaxis_var, "Please select a Y-Axis Variable"))
      param <- input$xaxis_param
      xaxis_var <- input$xaxis_var
      yaxis_var <- input$yaxis_var
      hline_vars <- input$hline_vars
      # nolint end
      teal.code::chunks_push(
        chunks = private_chunks,
        id = "g_spaghettiplot",
        expression = bquote({
          p <- goshawk::g_spaghettiplot(
            data = ANL,
            subj_id = .(idvar),
            biomarker_var = .(param_var),
            biomarker_var_label = .(param_var_label),
            biomarker = .(param),
            value_var = .(yaxis_var),
            trt_group = .(trt_group),
            trt_group_level = .(trt_group_level),
            time = .(xaxis_var),
            time_level = .(xaxis_var_level),
            color_manual = .(man_color),
            color_comb = .(color_comb),
            ylim = .(ylim),
            facet_ncol = .(facet_ncol),
            hline_arb = .(hline_arb),
            hline_arb_label = .(hline_arb_label),
            hline_arb_color = .(hline_arb_color),
            xtick = .(xtick),
            xlabel = .(xlabel),
            rotate_xlab = .(rotate_xlab),
            font_size = .(font_size),
            alpha = .(alpha),
            group_stats = .(group_stats),
            hline_vars = .(hline_vars),
            hline_vars_colors = .(hline_vars_colors[seq_along(hline_vars)]),
            hline_vars_labels = .(hline_vars_labels[seq_along(hline_vars)])
          )
          print(p)
        })
      )

      teal.code::chunks_safe_eval(private_chunks)

      # promote chunks to be visible in the sessionData by other modules
      teal.code::chunks_reset()
      teal.code::chunks_push_chunks(private_chunks)

      teal.code::chunks_get_var("p")
    })

    plot_data <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Spaghetti plot")
        card$append_text("Spaghetti plot", "header2")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Data constraint", "header3")
        card$append_text(
          formatted_data_constraint(input$constraint_var, input$constraint_range_min, input$constraint_range_max)
        )
        card$append_text("Spaghetti plot", "header3")
        card$append_plot(plot_r(), dim = plot_data$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_text("Show R Code", "header3")
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 1L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }
      teal.reporter::add_card_button_srv("addReportCard", reporter = reporter, card_fun = card_fun)
      teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
      teal.reporter::reset_report_button_srv("resetButton", reporter)
    }
    ###

    output$brush_data <- DT::renderDataTable({
      plot_brush <- plot_data$brush()

      ANL <- isolate(anl_chunks()$ANL) # nolint
      validate_has_data(ANL, 1)

      xvar <- isolate(input$xaxis_var)
      yvar <- isolate(input$yaxis_var)
      trt_group <- isolate(input$trt_group)

      req(all(c(xvar, yvar) %in% names(ANL)))

      df <- teal.widgets::clean_brushedPoints(
        dplyr::select(ANL, "USUBJID", trt_group, "PARAMCD", xvar, yvar, "LOQFL"),
        plot_brush
      )
      df <- df[order(df$PARAMCD, df[[trt_group]], df$USUBJID, df[[xvar]]), ]
      numeric_cols <- names(dplyr::select_if(df, is.numeric))

      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
        DT::formatRound(numeric_cols, 4)
    })

    get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      modal_title = "Spaghetti Plot"
    )
  })
}
