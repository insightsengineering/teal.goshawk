templ_ui_constraint <- function(ns, label = "Data Constraint") {
  tags$div(
    id = ns("constraint_var_whole"), # give an id to hide it
    radioButtons(ns("constraint_var"), label, choices = "NONE"),
    shinyjs::hidden(tags$div(
      id = ns("constraint_range"),
      tags$div(
        class = "inline-block;",
        numericInput(ns("constraint_range_min"), label = "Min", value = 0, min = 0, max = 0)
      ),
      tags$div(
        class = "inline-block;",
        numericInput(ns("constraint_range_max"), label = "Min", value = 0, min = 0, max = 0)
      )
    )),
    shinyjs::hidden(tags$div(
      id = ns("all_na"),
      helpText("All values are missing (NA)")
    ))
  )
}

keep_data_const_opts_updated <- function(session, input, data, id_param_var) {
  # use reactiveVal so that it only updates when options actually changed and not just data
  choices <- reactiveVal()
  observeEvent(data(), {
    paramname <- input[[id_param_var]]
    req(length(paramname) == 1)

    data_filtered <- data()$ANL %>% dplyr::filter(.data$PARAMCD == paramname)
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




# param_id: input id that contains values of PARAMCD to filter for
# param_var: currently only "PARAMCD" is supported
constr_anl_q <- function(session, input, data, dataname, param_id, param_var, trt_group, min_rows) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  dataset_var <- dataname
  if (!identical(param_var, "PARAMCD")) {
    # why is there a variable param_id which is provided to this function and always equal to "param"?
    stop("param_var must be 'PARAMCD'. Otherwise, we cannot currently guarantee the correctness of the code.")
  }

  anl_param <- reactive({
    param_var_value <- input[[param_id]] # value to filter PARAMCD for
    validate(need(param_var_value, "Please select a biomarker"))
    checkmate::assert_string(param_var_value)

    ANL <- data()[[dataname]] # nolint
    validate_has_data(ANL, min_rows)

    validate_has_variable(ANL, param_var)
    validate_has_variable(ANL, "AVISITCD")
    validate_has_variable(ANL, "BASE")
    validate_has_variable(ANL, "BASE2")
    validate_has_variable(ANL, trt_group)

    # analysis
    private_qenv <- data() %>%
      teal.code::eval_code(
        substitute(ANL <- dataname, list(dataname = as.name(dataname))) # nolint
      ) %>%
      teal.code::eval_code(
        code = bquote({
          ANL <- .(as.name(dataset_var)) %>% # nolint
            dplyr::filter(.(as.name(param_var)) == .(param_var_value))
        })
      )
    validate_has_data(private_qenv[["ANL"]], min_rows)
    list(ANL = ANL, qenv = private_qenv)
  })

  observe({
    param_var_value <- input[[param_id]]
    validate(need(param_var_value, "Please select a biomarker"))

    constraint_var <- input[["constraint_var"]]
    validate(need(constraint_var, "select a constraint variable"))

    ANL <- data()[[dataname]] # nolint
    validate_has_data(ANL, min_rows)

    validate_has_variable(ANL, param_var)
    validate_has_variable(ANL, "AVISITCD")
    validate_has_variable(ANL, "BASE")
    validate_has_variable(ANL, "BASE2")

    ANL <- ANL %>% dplyr::filter(!!sym(param_var) == param_var_value) # nolint

    visit_freq <- unique(ANL$AVISITCD)

    # get min max values
    if ((constraint_var == "BASE2" && any(grepl("SCR", visit_freq))) ||
      (constraint_var == "BASE" && any(grepl("BL", visit_freq)))) { # nolint
      val <- stats::na.omit(switch(constraint_var,
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
      validate(need(FALSE, "This is an invalid data contraint for the filtered data"))
    }
  })

  create_anl_constraint_reactive(anl_param, input, param_id = param_id, min_rows = min_rows)
}

# returns a reactive that applies the `x-axis data constraint`
# More precisely, all patients are filtered out that do not have the range of
# `param_id.constraint_var` in the specified range
# constraint var means that `param_id.constraint_var` is constrained to the filtered range (or NA),
# e.g. `ALT.BASE2` (i.e. `PARAMCD = ALT & range_filter_on(BASE2)`)
create_anl_constraint_reactive <- function(anl_param, input, param_id, min_rows) {
  iv_r <- reactive({
    iv <- shinyvalidate::InputValidator$new()
    iv$condition(~ isTRUE(input$constraint_var != "NONE"))
    iv$add_rule("constraint_range_min", shinyvalidate::sv_required("A contraint minimum value is required"))
    iv$add_rule("constraint_range_max", shinyvalidate::sv_required("A contraint maximum value is required"))
    iv$add_rule(
      "constraint_range_min",
      ~ if (!is.na(input$constraint_range_max) && (.) > input$constraint_range_max) {
        "constraint min needs to be less than max"
      }
    )
    iv$add_rule(
      "constraint_range_max",
      ~ if (!is.na(input$constraint_range_min) && (.) < input$constraint_range_min) {
        "constraint min needs to be less than max"
      }
    )
    iv
  })


  return_val <- reactive({
    private_qenv <- anl_param()$qenv

    # it is assumed that constraint_var is triggering constraint_range which then trigger this clause
    constraint_var <- isolate(input[["constraint_var"]])
    constraint_range_min <- input[["constraint_range_min"]]
    constraint_range_max <- input[["constraint_range_max"]]
    param <- input[[param_id]]
    req(param)

    # filter constraint
    if (constraint_var != "NONE") {
      private_qenv <- teal.code::eval_code(
        object = private_qenv,
        code = bquote({
          # the below includes patients who have at least one non-NA BASE value
          # ideally, a patient should either have all NA values or none at all
          # this could be achieved through preprocessing; otherwise, this is easily overseen
          filtered_usubjids <- ANL %>% # nolint
            dplyr::filter(
              PARAMCD == .(param),
              (.(constraint_range_min) <= .data[[.(constraint_var)]]) &
                (.data[[.(constraint_var)]] <= .(constraint_range_max))
            ) %>%
            dplyr::pull(USUBJID)
          # include patients with all NA values for constraint_var
          filtered_usubjids <- c(
            filtered_usubjids,
            ANL %>%
              dplyr::filter(PARAMCD == .(param)) %>%
              dplyr::group_by(USUBJID) %>%
              dplyr::summarize(all_na = all(is.na(.data[[.(constraint_var)]]))) %>%
              dplyr::filter(all_na) %>%
              dplyr::pull(USUBJID)
          )
          ANL <- ANL %>% dplyr::filter(USUBJID %in% filtered_usubjids) # nolint
        })
      )
      validate_has_data(private_qenv[["ANL"]], min_rows)
    }
    list(ANL = private_qenv[["ANL"]], qenv = private_qenv)
  })

  reactive(
    list(
      value = return_val,
      iv_r = iv_r
    )
  )
}

# for outputting the constraint in the report
formatted_data_constraint <- function(constraint_var, constraint_range_min, constraint_range_max) {
  constraint_var_label <- switch(constraint_var,
    "BASE2" = "Screening",
    "BASE" = "Baseline",
    "None"
  )
  msg <- paste("Data Constraint:", constraint_var_label)
  if (constraint_var_label != "None") {
    msg <- paste(msg, "from", constraint_range_min, "to", constraint_range_max)
  }
  msg
}

update_min_max <- function(session, args) {
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_min"), args$min))
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_max"), args$max))
}
