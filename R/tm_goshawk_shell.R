#' A simple module that allows for constraining
#'
#' @noRd
#'
#' @examples
#' dat <- teal.goshawk:::goshawk_data()
#'
#' app <- teal::init(
#'    data = cdisc_data(
#'     cdisc_dataset("ADSL", dat$ADSL),
#'     cdisc_dataset("ADLB", dat$ADLB),
#'     code = dat$code
#'    ),
#'    modules = root_modules(
#'      teal.goshawk:::tm_goshawk_shell("demo", "ADLB", "PARAMCD", choices_selected(c("ALT", "CRP", "IGA"), "ALT"))
#'    )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_goshawk_shell <- function(label, dataname, param_var, param, trt_group = "ARM") {

  stopifnot(is.choices_selected(param))

  module(
    label = label,
    server = srv_goshawk_shell,
    ui = ui_goshawk_shell,
    server_args = list(dataname = dataname, param_var = param_var, trt_group = trt_group),
    ui_args = list(param_var = param_var, param = param),
    filters = dataname
  )

}

ui_goshawk_shell <- function(id, datasets, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = textOutput(ns("txt")),
    encoding =  div(
      # those two templ_* functions need to be here for the server function to work
      templ_ui_param(ns, a$param$choices, a$param$selected),
      templ_ui_constraint(ns)
    ),
    forms = get_rcode_ui(ns("rcode"))
  )
}

srv_goshawk_shell <- function(input, output, session, datasets, dataname, param_var, trt_group) {

  anl_chunks <- constr_anl_chunks(session, input, datasets, dataname, param_var, trt_group)

  output$txt <- renderText({

    anl_chunks <- anl_chunks()

    ANL <- anl_chunks$ANL # nolint
    private_chunks <- anl_chunks$chunks$clone(deep = TRUE)

    init_chunks(private_chunks)

    dim(ANL)
  })


  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = "Scatter Plot"
  )
}