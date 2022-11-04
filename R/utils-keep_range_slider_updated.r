keep_range_slider_updated <- function(session,
                                      input,
                                      update_slider_fcn,
                                      id_var,
                                      id_param_var,
                                      reactive_ANL, # nolint
                                      is_density = FALSE) {
  stopifnot(is.function(update_slider_fcn))

  observe({
    varname <- input[[id_var]]
    validate(need(varname, "Please select variable"))
    paramname <- input[[id_param_var]]
    validate(need(paramname, "Please select variable"))
    stopifnot(length(paramname) == 1)

    # we need id_param_var (e.g. ALT) to filter down because the y-axis may have a different
    # param var and the range of id_var (e.g. BASE) values may be larger due to this
    # therefore, we need to filter
    ANL <- reactive_ANL()$ANL %>% dplyr::filter(.data$PARAMCD == paramname) # nolint
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

    var <- stats::na.omit(ANL[[varname]])
    minmax <- if (length(var)) c(floor(min(var)), ceiling(max(var))) else c(0, 0)
    step <- NULL

    if (isTRUE(is_density)) {
      minmax <- c(0, round(max(stats::density(stats::na.omit(ANL[[varname]]))$y) * 1.5, 5))
      step <- round(max(stats::density(stats::na.omit(ANL[[varname]]))$y) / 100, 5)
    }

    isolate(update_slider_fcn(
      min = minmax[[1]],
      max = minmax[[2]],
      value = minmax,
      step = step
    ))
  })
}
