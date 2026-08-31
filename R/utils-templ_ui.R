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
                                   facet_var = NULL,
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
    },
    if (!is.null(facet_var)) {
      tags$div(tags$label(facet_label), teal.picks::picks_ui(ns("facet_var"), facet_var))
    }
  )
}
