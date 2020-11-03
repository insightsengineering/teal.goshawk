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
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor
#' level of trt_group.
#' @param man_color string vector representing customized colors
#' @param color_comb name or hex value for combined treatment color.
#' @param hline numeric value to add horizontal line to plot
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
#' @inheritParams teal.devel::standard_layout
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
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD",
#'                     "B: Placebo" = "Placebo",
#'                     "C: Combination" = "Combination")
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
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
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset(
#'       "ADLB",
#'       ADLB,
#'       code = "ADLB <- radlb(cached = TRUE)
#'               ADLB <- ADLB %>%
#'                 mutate(AVISITCD = case_when(
#'                     AVISIT == 'SCREENING' ~ 'SCR',
#'                     AVISIT == 'BASELINE' ~ 'BL',
#'                     grepl('WEEK', AVISIT) ~
#'                       paste('W', stringr::str_extract(AVISIT, '(?<=(WEEK ))[0-9]+')),
#'                     TRUE ~ as.character(NA)),
#'                   AVISITCDN = case_when(
#'                     AVISITCD == 'SCR' ~ -2,
#'                     AVISITCD == 'BL' ~ 0,
#'                     grepl('W', AVISITCD) ~ as.numeric(gsub('[^0-9]*', '', AVISITCD)),
#'                     TRUE ~ as.numeric(NA)),
#'                   AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'                   TRTORD = case_when(
#'                     ARMCD == 'ARM C' ~ 1,
#'                     ARMCD == 'ARM B' ~ 2,
#'                     ARMCD == 'ARM A' ~ 3),
#'                   ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'                   ARM = factor(ARM) %>% reorder(TRTORD))",
#'       vars = list(arm_mapping = arm_mapping)),
#'     check = TRUE
#'     ),
#'   modules = root_modules(
#'     tm_g_gh_spaghettiplot(
#'       label = "Spaghetti Plot",
#'       dataname = "ADLB",
#'       param_var = "PARAMCD",
#'       param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       idvar = "USUBJID",
#'       xaxis_var = choices_selected(c("Analysis Visit Code" = "AVISITCD"), "AVISITCD"),
#'       yaxis_var = choices_selected(c("AVAL","CHG", "PCHG"), "AVAL"),
#'       filter_var = choices_selected(c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE"),
#'        "NONE"),
#'       trt_group = "ARM",
#'       color_comb = "#39ff14",
#'       man_color = c('Combination' = "#000000",
#'                    'Placebo' = "#fce300",
#'                    '150mg QD' = "#5a2f5f")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#'
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
                                  hline = NULL,
                                  man_color = NULL,
                                  color_comb = NULL,
                                  xtick = waiver(),
                                  xlabel = xtick,
                                  rotate_xlab = FALSE,
                                  facet_ncol = 2,
                                  plot_height = c(600, 200, 2000),
                                  plot_width = NULL,
                                  font_size = c(12, 8, 20),
                                  pre_output = NULL,
                                  post_output = NULL) {

  stopifnot(is.choices_selected(param))
  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

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
      plot_width = plot_width
    ),
    ui = g_ui_spaghettiplot,
    ui_args = args,
    filters = dataname
  )

}

g_ui_spaghettiplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = templ_ui_output_datatable(ns, a$plot_height, a$plot_width),
    encoding = div(
      templ_ui_dataname(a$dataname),
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
        inline = TRUE),
      templ_ui_constraint(ns), # required by constr_anl_chunks
      toggle_slider_ui(
        ns("yrange_scale"),
        label = "Y-Axis Range Zoom",
        min = -1000000,
        max = 1000000,
        value = c(-1000000, 1000000)),
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          div(style = "padding: 0px;",
            div(style = "display: inline-block;vertical-align:middle; width: 175px;",
              tags$b("Number of Plots Per Row:")),
            div(style = "display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("facet_ncol"), "", a$facet_ncol, min = 1))
          ),
          checkboxInput(ns("rotate_xlab"), "Rotate X-Axis Label", a$rotate_xlab),
          div(style = "padding: 0px;",
            div(style = "display: inline-block;vertical-align:moddle; width: 175px;",
              tags$b("Add a Horizontal Line:")),
            div(style = "display: inline-block;vertical-align:middle; width: 100px;",
              numericInput(ns("hline"), "", a$hline))
          ),
          optionalSliderInputValMinMax(ns("font_size"), "Font Size", a$font_size, ticks = FALSE),
          optionalSliderInputValMinMax(
            ns("alpha"),
            "Line Alpha",
            a$alpha,
            value_min_max =  c(0.8, 0.0, 1.0), step = 0.1, ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}



srv_g_spaghettiplot <- function(input,
                                output,
                                session,
                                datasets,
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
                                plot_width) {

  # reused in all modules
  anl_chunks <- constr_anl_chunks(
    session, input, datasets, dataname,
    param_id = "xaxis_param", param_var = param_var, trt_group = trt_group
  )

  # update sliders for axes taking constraints into account
  yrange_slider <- callModule(toggle_slider_server, "yrange_scale")
  keep_range_slider_updated(session, input, yrange_slider$update_state, "yaxis_var", "xaxis_param", anl_chunks)
  keep_data_const_opts_updated(session, input, anl_chunks, "xaxis_param")

  plot_r <- reactive({
    # nolint start
    private_chunks <- anl_chunks()$chunks$clone(deep = TRUE)
    ylim <- yrange_slider$state()$value
    facet_ncol <- input$facet_ncol
    rotate_xlab <- input$rotate_xlab
    hline <- input$hline
    group_stats <- input$group_stats
    font_size <- input$font_size
    alpha <- input$alpha

    # Below inputs should trigger plot via updates of other reactive objects (i.e. anl_chunk()) and some inputs
    validate(need(input$xaxis_var, "Please select an X-Axis Variable"))
    validate(need(input$yaxis_var, "Please select a Y-Axis Variable"))
    param <- input$xaxis_param
    xaxis_var <- input$xaxis_var
    yaxis_var <- input$yaxis_var
    # nolint end
    chunks_push(
      chunks = private_chunks,
      id = "g_spaghettiplot",
      expression = bquote({
        p <- g_spaghettiplot(
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
          hline = .(`if`(is.na(hline), NULL, as.numeric(hline))),
          xtick = .(xtick),
          xlabel = .(xlabel),
          rotate_xlab = .(rotate_xlab),
          font_size = .(font_size),
          alpha = .(alpha),
          group_stats = .(group_stats)
        )
        print(p)
      })
    )

    chunks_safe_eval(private_chunks)

    # promote chunks to be visible in the sessionData by other modules
    init_chunks(private_chunks)

    chunks_get_var("p")
  })

  plot_data <- callModule(
    plot_with_settings_srv,
    id = "plot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  output$brush_data <- DT::renderDataTable({
    plot_brush <- plot_data$brush()

    ANL <- isolate(anl_chunks()$ANL) # nolint
    validate_has_data(ANL, 5)

    xvar <- isolate(input$xaxis_var)
    yvar <- isolate(input$yaxis_var)

    req(all(c(xvar, yvar) %in% names(ANL)))

    df <- brushedPoints(
      select(ANL, "USUBJID", trt_group, "PARAMCD", xvar, yvar, "LOQFL"),
      plot_brush
    )
    df <- df[order(df$PARAMCD, df[[trt_group]], df$USUBJID, df[[xvar]]), ]
    numeric_cols <- names(select_if(df, is.numeric))

    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(numeric_cols, 4)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Spaghetti Plot"
  )

}
