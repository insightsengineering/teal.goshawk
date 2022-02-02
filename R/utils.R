templ_ui_output_datatable <- function(ns) {
  div(
    plot_with_settings_ui(id = ns("plot")),
    br(), hr(),
    h4("Selected Data Points"),
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
      optionalSelectInput(
        ns("xaxis_param"),
        `if`(is.null(xparam_label), "Select an X-Axis Biomarker", xparam_label),
        xparam_choices,
        `if`(is.null(xparam_selected), xparam_choices[1], xparam_selected),
        multiple = FALSE
      )
    },
    if (!is.null(xchoices)) {
      optionalSelectInput(
        ns("xaxis_var"),
        `if`(is.null(xvar_label), "Select an X-Axis Variable", xvar_label),
        xchoices, xselected,
        multiple = multiple
      )
    },
    if (!is.null(yparam_choices)) {
      optionalSelectInput(
        ns("yaxis_param"),
        `if`(is.null(yparam_label), "Select an Y-Axis Biomarker", yparam_label),
        yparam_choices,
        `if`(is.null(yparam_selected), yparam_choices[1], yparam_selected),
        multiple = FALSE
      )
    },
    if (!is.null(ychoices)) {
      optionalSelectInput(
        ns("yaxis_var"),
        `if`(is.null(yvar_label), "Select a Y-Axis Variable", yvar_label),
        ychoices, yselected,
        multiple = multiple
      )
    }
  )
}

keep_data_const_opts_updated <- function(session, input, data, id_param_var) {
  # use reactiveVal so that it only updates when options actually changed and not just data
  choices <- reactiveVal()
  observeEvent(data(), {
    paramname <- input[[id_param_var]]
    stopifnot(length(paramname) == 1)

    data_filtered <- data()$ANL %>% filter(.data$PARAMCD == paramname)
    choices(c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")[
      c(TRUE, !all(is.na(data_filtered[["BASE2"]])), !all(is.na(data_filtered[["BASE"]])))
    ])
  })
  observeEvent(choices(), {
    updateRadioButtons(session, "constraint_var", choices = choices())
    # hide when only one option "NONE"
    if (length(choices()) == 1) {
      shinyjs::hide("constraint_var_whole")
    } else {
      shinyjs::show("constraint_var_whole")
    }
  })
}

#' @importFrom shinyjs hidden
templ_ui_constraint <- function(ns, label = "Data Constraint") {
  div(
    id = ns("constraint_var_whole"), # give an id to hide it
    radioButtons(ns("constraint_var"), label, choices = "NONE"),
    shinyjs::hidden(div(
      id = ns("constraint_range"),
      div(
        style = "display: inline-block; vertical-align:center",
        numericInput(ns("constraint_range_min"), label = "Min", value = 0, min = 0, max = 0)
      ),
      div(
        style = "display: inline-block; vertical-align:center",
        numericInput(ns("constraint_range_max"), label = "Min", value = 0, min = 0, max = 0)
      )
    )),
    shinyjs::hidden(div(
      id = ns("all_na"),
      helpText("All values are missing (NA)")
    ))
  )
}

keep_range_slider_updated <- function(session, input, update_slider_fcn, id_var, id_param_var, reactive_ANL) { # nolint
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
    ANL <- reactive_ANL()$ANL %>% filter(.data$PARAMCD == paramname) # nolint
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

    var <- na.omit(ANL[[varname]])
    minmax <- if (length(var)) c(floor(min(var)), ceiling(max(var))) else c(0, 0)

    isolate(update_slider_fcn(
      min = minmax[[1]],
      max = minmax[[2]],
      value = minmax
    ))
  })
}

# param_id: input id that contains values of PARAMCD to filter for
# param_var: currently only "PARAMCD" is supported
#' @importFrom dplyr filter sym
#' @importFrom shinyjs hide show
constr_anl_chunks <- function(session, input, datasets, dataname, param_id, param_var, trt_group, min_rows) {
  dataset_var <- paste0(dataname, "_FILTERED")
  if (!identical(param_var, "PARAMCD")) {
    # why is there a variable param_id which is provided to this function and always equal to "param"?
    stop("param_var must be 'PARAMCD'. Otherwise, we cannot currently guarantee the correctness of the code.")
  }

  anl_param <- reactive({
    param_var_value <- input[[param_id]] # value to filter PARAMCD for
    validate(need(param_var_value, "Please select a biomarker"))
    checkmate::assert_string(param_var_value)

    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE) # nolint
    validate_has_data(ANL_FILTERED, min_rows)

    validate_has_variable(ANL_FILTERED, param_var)
    validate_has_variable(ANL_FILTERED, "AVISITCD")
    validate_has_variable(ANL_FILTERED, "BASE")
    validate_has_variable(ANL_FILTERED, "BASE2")
    validate_has_variable(ANL_FILTERED, trt_group)

    # analysis
    private_chunks <- chunks$new()
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)), private_chunks)

    # filter biomarker
    chunks_push(
      chunks = private_chunks,
      id = "filter_biomarker",
      expression = bquote({
        ANL <- .(as.name(dataset_var)) %>% # nolint
          dplyr::filter(.(as.name(param_var)) == .(param_var_value))
      })
    )

    ANL <- chunks_safe_eval(private_chunks) # nolint
    validate_has_data(ANL, min_rows)

    return(list(ANL = ANL, chunks = private_chunks))
  })

  observe({
    param_var_value <- input[[param_id]]
    validate(need(param_var_value, "Please select a biomarker"))

    constraint_var <- input[["constraint_var"]]
    validate(need(constraint_var, "select a constraint variable"))

    # note that filtered is false thus we cannot use anl_param()$ANL
    ANL <- datasets$get_data(dataname, filtered = FALSE) # nolint

    validate_has_variable(ANL, param_var)
    validate_has_variable(ANL, "AVISITCD")
    validate_has_variable(ANL, "BASE")
    validate_has_variable(ANL, "BASE2")

    ANL <- ANL %>% filter(!!sym(param_var) == param_var_value) # nolint

    visit_freq <- unique(ANL$AVISITCD)


    # get min max values
    if ((constraint_var == "BASE2" && any(grepl("SCR", visit_freq))) ||
      (constraint_var == "BASE" && any(grepl("BL", visit_freq)))) {
      val <- na.omit(switch(constraint_var,
        "BASE" = ANL$BASE[ANL$AVISITCD == "BL"],
        "BASE2" = ANL$BASE2[ANL$AVISITCD == "SCR"],
        stop(paste(constraint_var, "not allowed"))
      ))

      if (length(val) == 0 || all(is.na(val))) {
        shinyjs::show("all_na")
        shinyjs::hide("constraint_range")
        args <- list(
          min = list(label = "Min", min = 0, max = 0, value = 0),
          max = list(label = "Max", min = 0, max = 0, value = 0)
        )
        update_min_max(session, args)
      } else {
        rng <- range(val, na.rm = TRUE)

        minmax <- c(floor(rng[1] * 1000) / 1000, ceiling(rng[2] * 1000) / 1000)

        label_min <- sprintf("Min (%s)", minmax[1])
        label_max <- sprintf("Max (%s)", minmax[2])

        args <- list(
          min = list(label = label_min, min = minmax[1], max = minmax[2], value = minmax[1]),
          max = list(label = label_max, min = minmax[1], max = minmax[2], value = minmax[2])
        )

        update_min_max(session, args)
        shinyjs::show("constraint_range") # update before show
        shinyjs::hide("all_na")
      }
    } else if (constraint_var == "NONE") {
      shinyjs::hide("constraint_range") # hide before update
      shinyjs::hide("all_na")

      # force update (and thus refresh) on different constraint_var -> pass unique value for each constraint_var name
      args <- list(
        min = list(label = "Min", min = 0, max = 0, value = 0),
        max = list(label = "Max", min = 0, max = 0, value = 0)
      )

      update_min_max(session, args)
    } else {
      stop("invalid contraint_var", constraint_var)
    }
  })

  anl_constraint <- create_anl_constraint_reactive(anl_param, input, param_id = param_id, min_rows = min_rows)

  return(anl_constraint)
}

# returns a reactive that applies the `x-axis data constraint`
# More precisely, all patients are filtered out that do not have the range of
# `param_id.constraint_var` in the specified range
# constraint var means that `param_id.constraint_var` is constrained to the filtered range (or NA),
# e.g. `ALT.BASE2` (i.e. `PARAMCD = ALT & range_filter_on(BASE2)`)
create_anl_constraint_reactive <- function(anl_param, input, param_id, min_rows) {
  reactive({
    private_chunks <- anl_param()$chunks$clone(deep = TRUE)

    # it is assumed that constraint_var is triggering constraint_range which then trigger this clause
    constraint_var <- isolate(input[["constraint_var"]])
    constraint_range_min <- input[["constraint_range_min"]]
    constraint_range_max <- input[["constraint_range_max"]]
    param <- input[[param_id]]
    checkmate::assert_string(param)

    validate(need(constraint_range_min, "please select proper constraint minimum value"))
    validate(need(constraint_range_max, "please select proper constraint maximum value"))
    validate(need(constraint_range_min <= constraint_range_max, "constraint min needs to be smaller than max"))

    # filter constraint
    if (constraint_var != "NONE") {
      chunks_push(
        chunks = private_chunks,
        id = "filter_constraint",
        expression = bquote({
          # the below includes patients who have at least one non-NA BASE value
          # ideally, a patient should either have all NA values or none at all
          # this could be achieved through preprocessing; otherwise, this is easily overseen
          filtered_usubjids <- ANL %>% # nolint
            dplyr::filter(
              PARAMCD == .(param),
              (.(constraint_range_min) <= .data[[.(constraint_var)]]) &
                (.data[[.(constraint_var)]] <= .(constraint_range_max))
            ) %>%
            pull(USUBJID)
          # include patients with all NA values for constraint_var
          filtered_usubjids <- c(
            filtered_usubjids,
            ANL %>%
              dplyr::filter(PARAMCD == .(param)) %>%
              group_by(USUBJID) %>%
              summarize(all_na = all(is.na(.data[[.(constraint_var)]]))) %>%
              filter(all_na) %>%
              pull(USUBJID)
          )
          ANL <- ANL %>% filter(USUBJID %in% filtered_usubjids) # nolint
        })
      )

      ANL <- chunks_safe_eval(private_chunks) # nolint
      validate_has_data(ANL, min_rows)
    }

    chunks_push_new_line(private_chunks)
    chunks_safe_eval(private_chunks)

    return(list(ANL = private_chunks$get("ANL"), chunks = private_chunks))
  })
}


update_min_max <- function(session, args) {
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_min"), args$min))
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_max"), args$max))
}

#' helper for writing arm mapping and ordering code.
#'
#' Provides lines of code for left hand side of arm mapping. user must provide right hand side
#'
#' @details SPA configure study specific pre-processing for deploying goshawk. writing the code for ARM mapping and
#' ordering is tedious. this function helps to get that started by providing the left hand side of the
#' mapping and ordering syntax. call the function and then copy and paste the resulting code from the console
#' into the app.R file.
#'
#' @param df_armvar the dataframe and column name containing treatment code. e.g. ADSL$ARMCD
#' @param code controls whether mapping or ordering code is written to console. Valid values: "M" and "O".
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' # get treatment mapping code
#' maptrt(df_armvar = ADSL$ARMCD, code = "M")
#'
#' # get treatment ordering code
#' maptrt(df_armvar = ADSL$ARMCD, code = "O")
maptrt <- function(df_armvar, code = c("M", "O")) {
  code <- match.arg(code)

  # get arm variable
  trtvar <- strsplit(deparse(substitute(df_armvar)), "[$]")[[1]][2]

  dftrt <- data.frame(unique(df_armvar)) %>%
    mutate(trt_mapping = paste0("\"", unique(df_armvar), "\"", " = \"\",")) %>%
    mutate(trt_ordering = paste0(eval(trtvar), " == \"", unique(df_armvar), "\"", " ~  ,"))

  if (toupper(code) == "M") {
    print(unname(dftrt["trt_mapping"]), row.names = FALSE)
  } else if (toupper(code) == "O") {
    print(unname(dftrt["trt_ordering"]), row.names = FALSE)
  }
}

#' UI module to arbitrary lines
#'
#' UI module to input either horizontal or vertical lines to a plot via comma separated values
#'
#' @param id (`character(1)`)\cr
#'  defining namespace of the `shiny` module.
#' @param line_arb (`numeric`)\cr
#'  default values for the `textInput` defining values of arbitrary lines
#' @param line_arb_color (`character`)\cr
#'  default values for the `textInput` defining colors of arbitrary lines
#' @param line_arb_label (`character`)\cr
#'  default values for the `textInput` defining labels of arbitrary lines
#' @param title (`character(1)`)\cr
#'  title of the arbitrary lines input. The default is "Arbitrary Horizontal Lines".
#' @return (`shiny.tag`) an input to define values, colors and labels for arbitrary
#' straight lines.
#' @keywords internal
ui_arbitrary_lines <- function(id, line_arb, line_arb_label, line_arb_color, title = "Arbitrary Horizontal Lines:") {
  ns <- NS(id)
  div(
    tags$b(title),
    textInput(ns("line_arb"), label = "Value:", value = paste(line_arb, collapse = ", ")),
    textInput(ns("line_arb_label"), label = "Label:", value = paste(line_arb_label, collapse = ", ")),
    textInput(ns("line_arb_color"), label = "Color:", value = paste(line_arb_color, collapse = ", "))
  )
}
#' Server module to arbitrary lines
#'
#' Server to validate and transform the comma separated values into vectors of values
#' to be passed into goshawk functions.
#' @inheritParams shiny::moduleServer
#' @return (`reactive`) returning a `list` containing `line_arb`, `line_arb_color`,
#'  `line_arb_label` which are validated and could be passed to `goshawk` plot functions.
#' @keywords internal
srv_arbitrary_lines <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      line_arb <- strsplit(input$line_arb, "\\s{0,},\\s{0,}")[[1]] %>%
        as.numeric()
      if ((length(line_arb) == 1 && is.na(line_arb)) || length(line_arb) == 0) {
        line_arb <- numeric(0)
        line_arb_label <- character(0)
        line_arb_color <- character(0)
      } else {
        validate(need(all(!is.na(line_arb)), "Invalid arbitrary line values"))

        line_arb_label <- strsplit(input$line_arb_label, "\\s{0,},\\s{0,}")[[1]] %>%
          trimws()
        line_arb_color <- strsplit(input$line_arb_color, "\\s{0,},\\s{0,}")[[1]] %>%
          trimws()
        if (length(line_arb_label) == 0) {
          line_arb_label <- ""
        } else {
          validate(
            need(
              length(line_arb_label) %in% c(1, length(line_arb)),
              "Line input error: number of labels should be equal to 1 or the number of values"
            )
          )
        }
        if (length(line_arb_color) == 0 || line_arb_color == "") {
          line_arb_color <- "red"
        } else {
          validate(
            need(
              length(line_arb_color) %in% c(1, length(line_arb)),
              "Line input error: number of colors should be equal to 1 or the number of values"
            )
          )
        }
      }
      list(line_arb = line_arb, line_arb_label = line_arb_label, line_arb_color = line_arb_color)
    })
  })
}

# to check the arbitrary line arguments
validate_line_arb_arg <- function(line_arb, line_arb_color, line_arb_label) {
  checkmate::assert_numeric(line_arb)
  if (length(line_arb) > 0) {
    checkmate::assert(
      checkmate::check_string(line_arb_color),
      checkmate::check_character(line_arb_color, len = length(line_arb))
    )
    checkmate::assert(
      checkmate::check_string(line_arb_label),
      checkmate::check_character(line_arb_label, len = length(line_arb))
    )
  }
}

# to check the variable line arguments
validate_line_vars_arg <- function(line_vars, line_vars_colors, line_vars_labels) {
  checkmate::assert_character(line_vars)
  if (length(line_vars) > 0) {
    checkmate::assert(
      checkmate::check_string(line_vars_colors),
      checkmate::check_character(line_vars_colors, len = length(line_vars))
    )
    checkmate::assert(
      checkmate::check_string(line_vars_labels),
      checkmate::check_character(line_vars_labels, len = length(line_vars))
    )
  }
}
