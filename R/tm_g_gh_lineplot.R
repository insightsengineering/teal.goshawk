#' Line plot
#'
#' This teal module renders the UI and calls the function that creates a line plot.
#'
#' @inheritParams teal.devel::standard_layout
#' @param label menu item label of the module in the teal app.
#' @param dataname analysis data passed to the data argument of teal init. E.g. ADaM structured
#' laboratory data frame ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker selected.
#' @param param_var_label single name of variable in analysis data that includes parameter labels.
#' @param xaxis_var single name of variable in analysis data that is used as x-axis in the plot for the
#' respective goshawk function.
#' @param xvar_level vector that can be used to define the factor level of xvar. Only use it when
#' xvar is character or factor.
#' @param filter_var data constraint variable.
#' @param filter_var_choices data constraint variable choices.
#' @param yaxis_var single name of variable in analysis data that is used as summary variable in the
#' respective gshawk function.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param trt_group_level vector that can be used to define factor level of trt_group.
#' @param shape_choices Vector with names of ADSL variables which can be used to change shape
#' @param color_manual string vector representing customized colors
#' @param stat string of statistics
#' @param hline numeric value to add horizontal line to plot
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric.
#' Default value is waive().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values.
#' Default value is waive().
#' @param rotate_xlab boolean value indicating whether to rotate x-axis labels.
#' @param plot_height numeric vectors to define the plot height.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control the position dodge of error bar
#'
#' @importFrom ggplot2 waiver
#' @importFrom grDevices extendrange
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
#' library(stringr)
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
#'     AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
#'     TRTORD = case_when(
#'       ARMCD == "ARM C" ~ 1,
#'       ARMCD == "ARM B" ~ 2,
#'       ARMCD == "ARM A" ~ 3),
#'     ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
#'     ARM = factor(ARM) %>% reorder(TRTORD))
#'
#'
#' app <- teal::init(
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
#'     tm_g_gh_lineplot(
#'       label = "Line Plot",
#'       dataname = "ADLB",
#'        param_var = "PARAMCD",
#'        param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
#'       shape_choices = c("SEX", "RACE"),
#'       xaxis_var =choices_selected("AVISITCD", "AVISITCD"),
#'       yaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
#'       trt_group = "ARM"
#'     )
#'   )
#' )
#'
#' shinyApp(app$ui, app$server)
#' }

tm_g_gh_lineplot <- function(label,
                             dataname,
                             param_var,
                             param,
                             param_var_label = "PARAM",
                             xaxis_var, yaxis_var,
                             xvar_level = NULL,
                             filter_var = yaxis_var,
                             filter_var_choices = filter_var,
                             trt_group,
                             trt_group_level = NULL,
                             shape_choices = NULL,
                             stat = "mean",
                             hline = NULL,
                             color_manual = NULL,
                             xtick = waiver(),
                             xlabel = xtick,
                             rotate_xlab = FALSE,
                             plot_height = c(600, 200, 2000),
                             font_size = c(12, 8, 20),
                             dodge = c(0.4, 0, 1),
                             pre_output = NULL,
                             post_output = NULL) {
  stopifnot(is.choices_selected(xaxis_var))
  stopifnot(is.choices_selected(yaxis_var))
  stopifnot(is.choices_selected(param))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_lineplot,
    server_args = list(dataname = dataname,
                       param_var = param_var,
                       trt_group = trt_group, color_manual = color_manual,
                       xvar_level = xvar_level,
                       trt_group_level = trt_group_level,
                       shape_choices = shape_choices,
                       param_var_label = param_var_label,
                       xtick = xtick,
                       xlabel = xlabel),
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
      templ_ui_dataname(a$dataname),
      templ_ui_param(ns, a$param$choices, a$param$selected), # required by constr_anl_chunks
      templ_ui_xy_vars(ns, a$xaxis_var$choices, a$xaxis_var$selected,
                       a$yaxis_var$choices, a$yaxis_var$selected),
      uiOutput(ns("shape_ui")),
      radioButtons(ns("stat"), "Select a Statistic:", c("mean","median"), a$stat),
      templ_ui_constraint(ns), # required by constr_anl_chunks
      panel_group(
        panel_item(
          title = "Plot Aesthetic Settings",
          sliderInput(ns("yrange_scale"), label = "Y-Axis Range Zoom", min = 0, max = 1, value = c(0, 1)),
          checkboxInput(ns("rotate_xlab"), "Rotate X-axis Label", a$rotate_xlab),
          numericInput(ns("hline"), "Add a horizontal line:", a$hline)
        ),
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("plot_height"), "Plot Height", a$plot_height, ticks = FALSE),
          optionalSliderInputValMinMax(ns("dodge"), "Error Bar Position Dodge", a$dodge, ticks = FALSE),
          optionalSliderInputValMinMax(ns("font_size"),  "Font Size", a$font_size, ticks = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_lineplot <- function(input,
                         output,
                         session,
                         datasets,
                         dataname,
                         param_var,
                         trt_group,
                         color_manual,
                         xvar_level,
                         trt_group_level,
                         shape_choices,
                         param_var_label,
                         xtick,
                         xlabel) {

  ns <- session$ns
  output$shape_ui <- renderUI({
    if(!is.null(shape_choices)){
      selectInput(ns("shape"), "Select Line Splitting Variable",
                  choices = c("None", shape_choices), selected = "None")
    }
  })

  anl_chunks <- constr_anl_chunks(session = session,
                                  input = input,
                                  datasets = datasets,
                                  dataname = dataname,
                                  param_id = "param",
                                  param_var =  param_var,
                                  trt_group = trt_group)

  # update sliders for axes
  observe({
    varname <- input[["yaxis_var"]]
    validate(need(varname, "Please select variable"))

    ANL <- anl_chunks()$ANL # nolint
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

    sum_data <- ANL %>%
      group_by_at(c(input$xaxis_var, trt_group)) %>%
      summarise(upper = if (input$stat == 'mean') {
        mean(!!sym(varname), na.rm = TRUE) +
          1.96 * sd(!!sym(varname), na.rm = TRUE) / sqrt(n())
      } else {
        quantile(!!sym(varname), 0.75, na.rm = TRUE)
      },
      lower = if (input$stat == 'mean') {
        mean(!!sym(varname), na.rm = TRUE) -
          1.96 * sd(!!sym(varname), na.rm = TRUE) / sqrt(n())
      } else {
        quantile(!!sym(varname), 0.25, na.rm = TRUE)
      })

    minmax <- grDevices::extendrange(
      r = c(floor(min(sum_data$lower, na.rm = TRUE) * 10) / 10,
            ceiling(max(sum_data$upper, na.rm = TRUE)) * 10) / 10,
      f = 0.05
    )

    updateSliderInput(
      session = session,
      inputId = "yrange_scale",
      min = minmax[1],
      max = minmax[2],
      value = minmax
    )
  })

  output$lineplot <- renderPlot({
    ac <- anl_chunks()
    private_chunks <- ac$chunks$clone(deep = TRUE)
    yrange_scale <- input$yrange_scale
    font_size <- input$font_size
    dodge <- input$dodge
    rotate_xlab <- input$rotate_xlab
    hline <- if (is.na(input$hline)) NULL else as.numeric(input$hline)
    median <- ifelse(input$stat=='median',TRUE, FALSE)
    plot_height <- input$plot_height

    param <- isolate(input$param)
    xaxis <- isolate(input$xaxis_var)
    yaxis <- isolate(input$yaxis_var)

    shape <- if (!(is.null(input$shape) || input$shape == "None")) {
      input$shape
    } else {
      NULL
    }

    chunks_push(
      chunks = private_chunks,
      id = "lineplot",
      expression = bquote({
        g_lineplot(
          data = ANL,
          biomarker_var = .(param_var),
          biomarker_var_label = .(param_var_label),
          biomarker = .(param),
          value_var = .(yaxis),
          ylim = .(yrange_scale),
          trt_group = .(trt_group),
          trt_group_level = .(trt_group_level),
          shape = .(shape),
          time = .(xaxis),
          time_level = .(xvar_level),
          color_manual = .(color_manual),
          median = .(median),
          hline = .(hline),
          xtick = .(xtick),
          xlabel = .(xlabel),
          rotate_xlab = .(rotate_xlab),
          font_size = .(font_size),
          dodge = .(dodge),
          plot_height = .(plot_height)
        )
      })
    )

    p <- chunks_safe_eval(private_chunks)
    init_chunks(private_chunks)

    p
  })

  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need  valid plot height"))
    plotOutput(ns("lineplot"), height = plot_height)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Line Plot"
  )
}
