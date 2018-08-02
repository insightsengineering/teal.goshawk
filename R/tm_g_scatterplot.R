



#' Teal module to plot kaplan meier curves with extrapolation superimposed
#'
#' give more details about the method
#'
#' @param label label of teal module in app
#'
#'
#' @author Nick
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' ASL <- rasl(400)
#'
#' ASL$BWT <- rnorm(400)
#' ASL$VARNUM <- rnorm(400)
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'        label = "Parametric KM Plot",
#'        dataname = "ASL",
#'        var = c("AGE", "BWT"),
#'        var_choices = which(sapply(ASL, is.numeric)) %>% names()
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#' }
#'
#'
tm_g_scatterplot <- function(label,
                             dataname,
                             var,
                             var_choices,
                             pre_output = NULL,
                             post_output = NULL) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )

}


ui_g_scatterplot <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = plotOutput(ns("plot")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("varnames"), "Variables to plot", a$var_choices, a$var, multiple = TRUE)
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}



srv_g_scatterplot <- function(input, output, session, datasets, dataname) {

  output$plot <- renderPlot({

    # retrieve reactive values
    ANL <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    varnames <- input$varnames

    # check if choices work

    # do analysis and create plot
    plot_data <- ANL[, varnames, drop=FALSE]


    plot(plot_data)

  })

}
