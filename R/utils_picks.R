#' Extract datanames from list of picks
#' @keywords internal
#' @noRd
.picks_datanames <- function(...) {
  x <- rlang::dots_list(...)
  checkmate::assert_list(x, c("picks", "NULL"))
  datanames_list <- lapply(x, function(x) {
    if (is.character(x$datasets$choices)) {
      x$datasets$choices
    } else {
      NULL
    }
  })

  if (any(vapply(datanames_list, is.null, logical(1)))) {
    "all"
  } else {
    unique(unlist(datanames_list))
  }
}

#' Coerce legacy `teal.transform` specs to [`teal.picks::variables()`] with deprecation
#'
#' If `x` is a legacy `choices_selected`, `filter_spec`, or `select_spec` object, it is converted
#' via [`teal.picks::as.picks()`]. Otherwise `x` must already inherit `"variables"`.
#'
#' @param x (`values`, `choices_selected` or `picks`) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#' @param null.ok (`logical(1)`) whether `NULL` is allowed.
#'
#' @keywords internal
#' @noRd
migrate_choices_selected_to_variables <- function(x, # nolint: object_length_linter
                                                  arg_name = checkmate::vname(x),
                                                  multiple = NULL,
                                                  null.ok = FALSE) { # nolint: object_name_linter.
  # nolint: object_name_linter.
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)
  checkmate::assert_flag(null.ok)
  if (inherits(x, "picks")) {
    return(x)
  }

  if (isTRUE(null.ok) && is.null(x)) {
    return(x)
  }
  legacy <- c("choices_selected", "filter_spec", "select_spec")
  if (inherits(x, legacy)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::variables()` (or a full `teal.picks::picks()` chain).",
        "Support for legacy `teal.transform::choices_selected()`, `filter_spec`, and `select_spec` is deprecated."
      )
    )
    x <- teal.picks::as.picks(x, quiet = FALSE)
    attr(x, "multiple") <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(x$selected) > 1L)
  } else {
    if (!is.null(multiple) && !identical(attr(x, "multiple", exact = TRUE), multiple)) {
      stop(
        sprintf("`multiple` metadata does not match the requirement for %s.", arg_name),
        sprintf(" Please set multiple = %s in the picks object.", multiple),
        call. = FALSE
      )
    }
  }
  checkmate::assert_class(
    x,
    "variables",
    null.ok = null.ok,
    .var.name = arg_name
  )
  x
}

#' Coerce legacy `choices_selected` to [`teal.picks::values()`] with deprecation
#'
#' @param x (`values`, `choices_selected`, [`teal.picks::picks()`], or [`teal.picks::variables()`]) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object. If `FALSE`, the result is checked with [teal.picks::is_pick_multiple()].
#'
#' @keywords internal
#' @noRd
#' @examples
#' migrate_choices_selected_to_values(choices_selected(c("A", "B"), selected = "A"))
migrate_choices_selected_to_values <- function(x, # nolint: object_length_linter.
                                               arg_name = checkmate::vname(x),
                                               multiple = NULL) {
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)

  if (inherits(x, "picks")) {
    return(x)
  }
  if (inherits(x, "variables")) {
    return(x)
  }
  if (inherits(x, "choices_selected")) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::values()`.",
        "Support for legacy `teal.transform::choices_selected()` is deprecated."
      )
    )
    if (is.null(x$choices) || inherits(x$choices, "delayed_data")) {
      stop(
        "Delayed `choices_selected` objects cannot be coerced automatically; ",
        "specify `teal.picks::values()` explicitly.",
        call. = FALSE
      )
    }
    choices <- as.character(x$choices)
    selected <- as.character(unlist(x$selected, use.names = FALSE))
    checkmate::assert_character(choices, min.len = 1L)
    if (length(selected) == 0) {
      selected <- NULL
    }
    checkmate::assert_character(selected, min.len = 1L, null.ok = TRUE)
    fixed <- isTRUE(x$fixed)
    multiple <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(selected) > 1L)
    x <- teal.picks::values(choices, selected, fixed = fixed, multiple = multiple)
  }
  checkmate::assert_class(x, "values", .var.name = arg_name)
  x
}

#' Supports the creation of picks object that does not override a dataset if already exists
#' @param datasets ([`teal.picks::datasets()`] object) to use if `x` does not already have a dataset.
#' @param x (`pick` or `picks` object) to ensure has a dataset.
#' @return a `picks` object with a dataset, either from `x` or from `datasets`.
#' @keywords internal
#' @noRd
create_picks_helper <- function(datasets = NULL, x, ...) {
  if (inherits(x, "picks") && !is.null(x$datasets)) {
    return(x)
  }
  checkmate::assert_class(datasets, "datasets", null.ok = FALSE)
  checkmate::assert_multi_class(x, c("pick", "picks"))

  if (inherits(x, "picks")) {
    picks_args <- c(list(datasets, x$variables, x$values), rlang::dots_list(...))
    do.call(
      teal.picks::picks,
      picks_args[vapply(picks_args, Negate(is.null), logical(1L))]
    )
  } else if (inherits(x, "pick")) {
    do.call(
      teal.picks::picks,
      c(list(datasets, x), rlang::dots_list(...))
    )
  }
}

#' Force a picks variable selection to single or multiple selection (internal)
#'
#' Warns when `pick$variables` allows multiple selection and coerces the
#' underlying metadata to `multiple = multiple` (default false).
#'
#' @param pick (`picks`)
#' @param arg_name (`character`) argument name used in warning messages.
#' @param multiple (`logical`) whether selection shuold be multiple (TRUE) or single (FALSE)
#'
#' @return The updated `pick` object.
#'
#' @keywords internal
#' @noRd
force_pick_selection <- function(pick,
                                 arg_name = checkmate::vname(pick),
                                 which = c("variables", "values", "datasets"),
                                 multiple = FALSE) {
  checkmate::assert_class(pick, "picks", .var.name = arg_name)
  checkmate::assert_string(arg_name)
  checkmate::assert_logical(multiple)
  which <- match.arg(which)
  selection <- if (isTRUE(multiple)) "multiple" else "single"

  if (!identical(multiple, teal.picks::is_pick_multiple(pick[[which]]))) {
    warning(
      sprintf(
        "`%s` accepts only a %s variable selection. \nForcing `teal.picks::%s(multiple)` to `%s`.",
        arg_name, selection, which, multiple
      )
    )
    attr(pick[[which]], "multiple") <- multiple
  }

  pick
}
