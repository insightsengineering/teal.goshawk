templ_ui_output_datatable <- function(ns) {
  tags$div(
    teal.widgets::plot_with_settings_ui(id = ns("plot")),
    tags$br(), tags$hr(),
    tags$h4("Selected Data Points"),
    DT::dataTableOutput(ns("brush_data"))
  )
}

templ_ui_dataname <- function(dataname) {
  tags$label(dataname, "Data Settings", class = "text-primary")
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

                                 multiple = FALSE) {
  if (is.null(xparam_choices) && !is.null(xchoices) && !is.null(yparam_choices)) {
    # otherwise, xchoices will appear first without any biomarker to select and this looks odd in the UI
    stop(
      "You have to specify xparam choices rather than yparamchoices
      if both xvar and yvar should be values for the same biomarker."
    )
  }
  tagList(
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
