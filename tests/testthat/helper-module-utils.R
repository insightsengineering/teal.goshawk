# nolint start
get_test_data <- function() {
  data <- teal_data()
  data <- within(data, {
    library(dplyr)
    library(nestcolor)
    library(stringr)

    # use non-exported function from goshawk
    .h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")

    # original ARM value = dose value
    .arm_mapping <- list(
      "A: Drug X" = "150mg QD",
      "B: Placebo" = "Placebo",
      "C: Combination" = "Combination"
    )
    set.seed(1)
    ADSL <- rADSL
    ADLB <- rADLB
    .var_labels <- lapply(ADLB, function(x) attributes(x)$label)
    ADLB <- ADLB %>%
      mutate(
        AVISITCD = case_when(
          AVISIT == "SCREENING" ~ "SCR",
          AVISIT == "BASELINE" ~ "BL",
          grepl("WEEK", AVISIT) ~ paste("W", str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
          TRUE ~ as.character(NA)
        ),
        AVISITCDN = case_when(
          AVISITCD == "SCR" ~ -2,
          AVISITCD == "BL" ~ 0,
          grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
          TRUE ~ as.numeric(NA)
        ),
        AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
        TRTORD = case_when(
          ARMCD == "ARM C" ~ 1,
          ARMCD == "ARM B" ~ 2,
          ARMCD == "ARM A" ~ 3
        ),
        ARM = as.character(.arm_mapping[match(ARM, names(.arm_mapping))]),
        ARM = factor(ARM) %>% reorder(TRTORD),
        ACTARM = as.character(.arm_mapping[match(ACTARM, names(.arm_mapping))]),
        ACTARM = factor(ACTARM) %>% reorder(TRTORD),
        ANRLO = 50,
        ANRHI = 75
      ) %>%
      rowwise() %>%
      group_by(PARAMCD) %>%
      mutate(LBSTRESC = ifelse(
        USUBJID %in% sample(USUBJID, 1, replace = TRUE),
        paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
      )) %>%
      mutate(LBSTRESC = ifelse(
        USUBJID %in% sample(USUBJID, 1, replace = TRUE),
        paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
      )) %>%
      ungroup()

    attr(ADLB[["ARM"]], "label") <- .var_labels[["ARM"]]
    attr(ADLB[["ACTARM"]], "label") <- .var_labels[["ACTARM"]]
    attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
    attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"

    # add LLOQ and ULOQ variables
    ALB_LOQS <- .h_identify_loq_values(ADLB, "LOQFL")
    ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
  })
  join_keys(data) <- default_cdisc_join_keys[names(data)]
  data
}
# nolint end

# Building the test data is slow, so it is cached for the whole test run.
.test_data_cache <- new.env(parent = emptyenv())
get_cached_test_data <- function() {
  if (is.null(.test_data_cache$data)) {
    .test_data_cache$data <- get_test_data()
  }
  .test_data_cache$data
}

#' Inputs that every goshawk module reads but that are not bound in `shiny::testServer()`.
#' @param ... named inputs overriding or extending the defaults.
default_module_inputs <- function(...) {
  utils::modifyList(
    list(
      constraint_var = "NONE",
      constraint_range_min = 0,
      constraint_range_max = 0,
      `hline_arb-line_arb` = "",
      `hline_arb-line_arb_label` = "",
      `hline_arb-line_arb_color` = ""
    ),
    list(...)
  )
}

#' Server arguments for `shiny::testServer()` based on a `teal_module`.
module_server_args <- function(mod, data = get_cached_test_data()) {
  c(list(id = "test", data = shiny::reactive(data)), mod$server_args)
}

#' Call the module UI with its arguments.
module_ui <- function(mod) {
  do.call(mod$ui, c(list(id = "test"), mod$ui_args))
}

#' Extract the code of a `qenv` as a single string.
code_of <- function(q) {
  paste(teal.code::get_code(q), collapse = "\n")
}

#' Wrap a module server so that inputs exist before it starts.
#'
#' In a running app the browser sends the initial input values before the server function
#' is called. `shiny::testServer()` only allows setting inputs after the module has started,
#' which fails for modules that read inputs while initializing.
#' @param server (`function`) module server function.
#' @param inputs (`list`) named list of initial (non-namespaced) inputs.
with_initial_inputs <- function(server, inputs) {
  function(id, ...) {
    session <- shiny::getDefaultReactiveDomain()
    do.call(session$setInputs, stats::setNames(inputs, paste0(id, "-", names(inputs))))
    server(id, ...)
  }
}
