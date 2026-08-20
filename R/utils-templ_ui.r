templ_ui_output_datatable <- function(ns) {
  bslib::page_fluid(
    teal.widgets::plot_with_settings_ui(id = ns("plot")),
    tags$br(), tags$hr(),
    tags$h4("Selected Data Points"),
    tags$div(
      DT::dataTableOutput(ns("brush_data"))
    )
  )
}

templ_ui_dataname <- function(dataname) {
  tags$label(dataname, "Data Settings", class = "text-primary")
}


tmpl_axis_selection_ui <- function(ns,
                                   xaxis_param = NULL,
                                   xaxis_var = NULL,
                                   yaxis_param = NULL,
                                   yaxis_var = NULL,
                                   facet = NULL,
                                   trt_group = NULL,
                                   xparam_label = "Select an X-Axis Biomarker",
                                   yparam_label = "Select an Y-Axis Biomarker",
                                   xvar_label = "Select an X-Axis Variable",
                                   yvar_label = "Select an Y-Axis Variable",
                                   facet_label = "Facet by",
                                   trt_label = "Select Treatment Variable") {
  tags$div(
    if (!is.null(trt_group)) {
      tags$div(tags$label(trt_label), teal.picks::picks_ui(ns("trt_group"), trt_group))
    },
    if (!is.null(xaxis_param)) {
      tags$div(tags$label(xparam_label), teal.picks::picks_ui(ns("xaxis_param"), xaxis_param))
    },
    if (!is.null(xaxis_var)) {
      tags$div(tags$label(xvar_label), teal.picks::picks_ui(ns("xaxis_var"), xaxis_var))
    },
    if (!is.null(yaxis_param)) {
      tags$div(tags$label(yparam_label), teal.picks::picks_ui(ns("yaxis_param"), yaxis_param))
    },
    if (!is.null(yaxis_var)) {
      tags$div(tags$label(yvar_label), teal.picks::picks_ui(ns("yaxis_var"), yaxis_var))
    }
  )
}

tmpl_axis_selection_selectors <- function(data,
                                          xaxis_param = NULL,
                                          xaxis_var = NULL,
                                          yaxis_param = NULL,
                                          yaxis_var = NULL,
                                          facet = NULL,
                                          trt_group = NULL,
                                          session = shiny::getDefaultReactiveDomain()) {
  teal.picks::picks_srv(
    id = "",
    picks = list(
      xaxis_param = xaxis_param,
      xaxis_var = xaxis_var,
      yaxis_param = yaxis_param,
      yaxis_var = yaxis_var,
      facet = facet,
      trt_group = trt_group
    ),
    data = data
  )
}

# UI to create params (biomarker, value of PARAMCD) and vars (column, e.g. AVAL column) select fields for x and y
templ_ui_params_vars <- function(ns,
                                 # x
                                 xparam_choices = NULL,
                                 xparam_selected = NULL,
                                 xparam_label = NULL, # biomarker, e.g. ALT
                                 xchoices = NULL,
                                 xselected = NULL,
                                 xvar_label = NULL, # variable, e.g. AVAL
                                 # y
                                 yparam_choices = NULL,
                                 yparam_selected = NULL,
                                 yparam_label = NULL, # biomarker, e.g. ALT
                                 ychoices = NULL,
                                 yselected = NULL,
                                 yvar_label = NULL, # variable, e.g. AVAL
                                 # facet_var
                                 facet_choices = NULL,
                                 facet_selected = NULL,
                                 # trt_group
                                 trt_choices = NULL,
                                 trt_selected = NULL,
                                 multiple = FALSE) {
  if (is.null(xparam_choices) && !is.null(xchoices) && !is.null(yparam_choices)) {
    # otherwise, xchoices will appear first without any biomarker to select and this looks odd in the UI
    stop(
      "You have to specify xparam choices rather than yparamchoices
      if both xvar and yvar should be values for the same biomarker."
    )
  }
  tagList(
    if (!is.null(trt_choices)) {
      teal.widgets::optionalSelectInput(
        ns("trt_group"),
        label = "Select Treatment Variable",
        choices = trt_choices,
        selected = trt_selected,
        multiple = FALSE
      )
    },
    if (!is.null(xparam_choices)) {
      teal.widgets::optionalSelectInput(
        ns("xaxis_param"),
        `if`(is.null(xparam_label), "Select an X-Axis Biomarker", xparam_label),
        xparam_choices,
        `if`(is.null(xparam_selected), xparam_choices[1], xparam_selected),
        multiple = FALSE
      )
    },
    if (!is.null(xchoices)) {
      teal.widgets::optionalSelectInput(
        ns("xaxis_var"),
        `if`(is.null(xvar_label), "Select an X-Axis Variable", xvar_label),
        xchoices, xselected,
        multiple = multiple
      )
    },
    if (!is.null(yparam_choices)) {
      teal.widgets::optionalSelectInput(
        ns("yaxis_param"),
        `if`(is.null(yparam_label), "Select an Y-Axis Biomarker", yparam_label),
        yparam_choices,
        `if`(is.null(yparam_selected), yparam_choices[1], yparam_selected),
        multiple = FALSE
      )
    },
    if (!is.null(ychoices)) {
      teal.widgets::optionalSelectInput(
        ns("yaxis_var"),
        `if`(is.null(yvar_label), "Select a Y-Axis Variable", yvar_label),
        ychoices, yselected,
        multiple = multiple
      )
    },
    if (!is.null(facet_choices)) {
      teal.widgets::optionalSelectInput(
        ns("facet_var"),
        label = "Facet by",
        choices = facet_choices,
        selected = facet_selected,
        multiple = FALSE
      )
    }
  )
}
