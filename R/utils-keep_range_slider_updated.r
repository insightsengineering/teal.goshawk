keep_range_slider_updated <- function(session,
                                      input,
                                      update_slider_fcn,
                                      id_var,
                                      id_param_var,
                                      reactive_ANL, # nolint
                                      is_density = FALSE,
                                      id_trt_group) {
  stopifnot(is.function(update_slider_fcn))

  observe({
    varname <- input[[id_var]]
    validate(need(varname, "Please select variable"))
    paramname <- input[[id_param_var]]
    validate(need(paramname, "Please select variable"))
    req(length(paramname) == 1)

    # we need id_param_var (e.g. ALT) to filter down because the y-axis may have a different
    # param var and the range of id_var (e.g. BASE) values may be larger due to this
    # therefore, we need to filter
    ANL <- reactive_ANL()$ANL %>% dplyr::filter(.data$PARAMCD == paramname) # nolint
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

    var <- stats::na.omit(ANL[[varname]])
    minmax <- if (length(var)) c(floor(min(var)), ceiling(max(var))) else c(0, 0)
    step <- NULL

    if (isTRUE(is_density)) {
      treatname <- input[[id_trt_group]]
      ANL_split <- ANL %>% split(f = factor(paste0(ANL[['AVISITCD']], ANL[[treatname]])))
      density_maxes <- lapply(ANL_split, function(x){
        max(stats::density(stats::na.omit(x[[varname]]))$y)
      })
      dmax <- max(unlist(density_maxes))
      minmax <- c(0, round(dmax * 1.2, 5))
      step <- round(dmax / 100, 5)
    }

    isolate(update_slider_fcn(
      min = minmax[[1]],
      max = minmax[[2]],
      value = minmax,
      step = step
    ))
  })
}
