#' Prepare input to \code{cdisc_data} for testing purposes
#'
#' @noRd
goshawk_data <- function() {

  # original ARM value = dose value
  arm_mapping <- list("A: Drug X" = "150mg QD",
                      "B: Placebo" = "Placebo",
                      "C: Combination" = "Combination")

  ADSL <- random.cdisc.data::radsl(N = 20, seed = 1) # nolint
  ADLB <- random.cdisc.data::radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2) # nolint
  ADLB <- ADLB %>% # nolint
    mutate(AVISITCD = case_when(
      .data$AVISIT == "SCREENING" ~ "SCR",
      .data$AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", .data$AVISIT) ~ paste("W", stringr::str_extract(.data$AVISIT, "(?<=(WEEK ))[0-9]+")),
      TRUE ~ as.character(NA)),
      AVISITCDN = case_when(
        .data$AVISITCD == "SCR" ~ -2,
        .data$AVISITCD == "BL" ~ 0,
        grepl("W", .data$AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", .data$AVISITCD)),
        TRUE ~ as.numeric(NA)),
      AVISITCD = factor(.data$AVISITCD) %>% reorder(.data$AVISITCDN),
      TRTORD = case_when(
        .data$ARMCD == "ARM C" ~ 1,
        .data$ARMCD == "ARM B" ~ 2,
        .data$ARMCD == "ARM A" ~ 3),
      ARM = as.character(arm_mapping[match(.data$ARM, names(arm_mapping))]),
      ARM = factor(.data$ARM) %>% reorder(.data$TRTORD))

  list(ADSL = ADSL, ADLB = ADLB, code = '
  arm_mapping <- list("A: Drug X" = "150mg QD",
                      "B: Placebo" = "Placebo",
                      "C: Combination" = "Combination")

  ADSL <- radsl(N = 20, seed = 1)
  ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
  ADLB <- ADLB %>%
    mutate(AVISITCD = case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
      TRUE ~ as.character(NA)),
      AVISITCDN = case_when(
        AVISITCD == "SCR" ~ -2,
        AVISITCD == "BL" ~ 0,
        grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
        TRUE ~ as.numeric(NA)),
      AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
      TRTORD = case_when(
        ARMCD == "ARM C" ~ 1,
        ARMCD == "ARM B" ~ 2,
        ARMCD == "ARM A" ~ 3),
      ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
      ARM = factor(ARM) %>% reorder(TRTORD))
       ')
}


templ_ui_output_datatable <- function(ns) {
  div(
    uiOutput(ns("plot_ui")),
    br(), hr(),
    h4("Selected Data Points"),
    DT::dataTableOutput(ns("brush_data"))
  )
}

templ_ui_dataname <- function(dataname) {
  tags$label(dataname, "Data Settings", class = "text-primary")
}
templ_ui_param <- function(ns, choices, selected) {
  selectInput(ns("param"), "Select a Biomarker", choices, selected, multiple = FALSE)
}

templ_ui_xy_vars <- function(ns, xchoices, xselected, ychoices, yselected, multiple = FALSE) {
  tagList(
    optionalSelectInput(ns("xaxis_var"), "Select an X-Axis Variable",  xchoices,  xselected, multiple = multiple),
    optionalSelectInput(ns("yaxis_var"), "Select a Y-Axis Variable", ychoices, yselected, multiple = multiple)
  )
}


#' @importFrom shinyjs hidden
templ_ui_constraint <- function(ns) {
  div(
    radioButtons(ns("constraint_var"),  "Data Constraint",
                 c("None" = "NONE", "Screening" = "BASE2", "Baseline" = "BASE")),
    shinyjs::hidden(div(
      id = ns("constraint_range"),
      div(
        style = "display: inline-block; vertical-align:center",
        numericInput(ns("constraint_range_min"), label = "Min", value = 0,  min = 0,  max = 0)
      ),
      div(
        style = "display: inline-block; vertical-align:center",
        numericInput(ns("constraint_range_max"), label = "Min", value = 0, min = 0, max = 0)
      )
    ))
  )
}


keep_range_slider_updated <- function(session, input, id_slider, id_var, reactive_ANL) { # nolint
  observe({
    varname <- input[[id_var]]
    validate(need(varname, "Please select variable"))

    ANL <- reactive_ANL()$ANL # nolint
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))

    vals <- ANL[[varname]]

    minmax <- c(
      floor(min(if_empty(na.omit(vals), 0))),
      ceiling(max(if_empty(na.omit(vals), 0)))
    )

    updateSliderInput(
      session = session,
      inputId = id_slider,
      min = minmax[1],
      max = minmax[2],
      value = minmax
    )
  })
}

#' @importFrom dplyr filter sym
#' @importFrom shinyjs hide show
constr_anl_chunks <- function(session, input, datasets, dataname, param_id, param_var, trt_group) {
  dataset_var <- paste0(dataname, "_FILTERED")

  anl_param <- reactive({
    param <- input[[param_id]]
    validate(need(param, "Please select a biomarker"))

    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # nolint
    validate_has_data(ANL_FILTERED, 5)

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
          dplyr::filter(.(as.name(param_var)) == .(param))
      })
    )

    ANL <- chunks_safe_eval(private_chunks) # nolint
    validate_has_data(ANL, 5)

    return(list(ANL = ANL, chunks = private_chunks))
  })

  observe({
    param <- input[[param_id]]
    validate(need(param, "Please select a biomarker"))

    constraint_var <- input$constraint_var
    validate(need(constraint_var, "select a constraint variable"))

    # note that filtered is false thus we cannot use anl_param()$ANL
    ANL <- datasets$get_data(dataname, filtered = FALSE, reactive = TRUE) # nolint

    validate_has_variable(ANL, param_var)
    validate_has_variable(ANL, "AVISITCD")
    validate_has_variable(ANL, "BASE")
    validate_has_variable(ANL, "BASE2")

    ANL <- ANL %>% filter(!!sym(param_var) == param) # nolint

    visit_freq <- unique(ANL$AVISITCD)

    # get min max values
    if ((constraint_var == "BASE2" & any(grepl("SCR", visit_freq))) ||
        (constraint_var == "BASE" & any(grepl("BL", visit_freq)))) {

      val <- na.omit(switch(
        constraint_var,
        "BASE" = ANL$BASE[ANL$AVISITCD == "BL"],
        "BASE2" = ANL$BASE2[ANL$AVISITCD == "SCR"],
        stop(paste(constraint_var, "not allowed"))
      ))

      validate_has_elements(val, "filtered data has no rows")

      rng <- range(val)

      minmax <- c(floor(rng[1] * 1000) / 1000,  ceiling(rng[2] * 1000) / 1000)

      label_min <- sprintf("Min (%s)", minmax[1])
      label_max <- sprintf("Max (%s)", minmax[2])

      args <- list(
        min = list(label = label_min, min = minmax[1], max = minmax[2], value = minmax[1]),
        max = list(label = label_max, min = minmax[1], max = minmax[2], value = minmax[2])
      )

      update_min_max(session, args)

      shinyjs::show("constraint_range") # update before show

    } else {

      shinyjs::hide("constraint_range") # hide before update

      # force update (and thus refresh) on different constraint_var -> pass unique value for each constraint_var name
      args <- list(
        min = list(label = "Min", min = 0, max = 0, value = 0),
        max = list(label = "Max", min = 0, max = 0, value = 0)
      )

      update_min_max(session, args)
    }
  })

  anl_constraint <- reactive({
    # it is assumed that constraint_var is triggering constraint_range which then trigger this clause
    constraint_var <- isolate(input$constraint_var)
    constraint_range_min <- input$constraint_range_min
    constraint_range_max <- input$constraint_range_max

    validate(need(constraint_range_min, "please select proper constraint minimum value"))
    validate(need(constraint_range_max, "please select proper constraint maximum value"))
    validate(need(constraint_range_min <= constraint_range_max, "constraint min needs to be smaller than max"))

    anl_param <- anl_param()
    private_chunks <- anl_param$chunks$clone(deep = TRUE)

    # filter constraint
    if (constraint_var != "NONE") {
      chunks_push(
        chunks = private_chunks,
        id = "filter_constraint",
        expression = bquote({
          ANL <- ANL %>% # nolint
            dplyr::filter(
              (.(constraint_range_min) <= .(as.name(constraint_var)) &
                 .(as.name(constraint_var)) <= .(constraint_range_max)) |
                is.na(.(as.name(constraint_var)))
            )
        })
      )
      ANL <- chunks_safe_eval(private_chunks) # nolint
      validate_has_data(ANL, 5)
    }

    chunks_push_new_line(private_chunks)
    chunks_safe_eval(private_chunks)

    return(list(ANL = chunks_get_var("ANL", private_chunks), chunks = private_chunks))
  })

  return(anl_constraint)
}


update_min_max <- function(session, args) {
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_min"), args$min))
  do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_max"), args$max))
}
