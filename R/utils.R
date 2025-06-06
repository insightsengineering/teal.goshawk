plots_per_row_validate_rules <- function(required = TRUE) {
  msg <- "Number of plots per row must be a positive integer"
  shinyvalidate::compose_rules(
    if (required) {
      shinyvalidate::sv_required(msg)
    } else {
      shinyvalidate::sv_optional()
    },
    shinyvalidate::sv_integer(msg),
    shinyvalidate::sv_gt(0, message_fmt = msg)
  )
}

#' Template Function for `TealReportCard` Creation and Customization in `teal.goshawk`
#'
#' This function generates a report card with a title,
#' an optional description, and the option to append the filter state list.
#' Additionally, it display selected constraint options.
#'
#' @inheritParams teal::report_card_template
#' @param constraint_list (`list`) a list containing constraint variables, including:
#'   - constraint_var (`character(1)`) the constraint variable name.
#'   - constraint_range_min (`numeric(1)`) the minimum constraint range value.
#'   - constraint_range_max (`numeric(1)`) the maximum constraint range value.
#' @param constraint_description (`character(1)`)  description of the constraints.
#' @param style (`character(1)`)  style of the constraint text block.
#'   options: `default`, `verbatim` (default is `default`).
#'
#' @return (`TealReportCard`) populated with a title, description, and filter state
#'
#' @keywords internal
report_card_template_goshawk <- function(title,
                                         label,
                                         with_filter,
                                         filter_panel_api,
                                         constraint_list,
                                         constraint_description = NULL,
                                         style = "default") {
  checkmate::assert_subset(names(constraint_list), c("constraint_var", "constraint_range_min", "constraint_range_max"))
  checkmate::assert_string(constraint_description, null.ok = TRUE)
  checkmate::assert_choice(style, c("default", "verbatim"))

  card <- teal::report_card_template(
    title = title,
    label = label,
    with_filter = with_filter,
    filter_panel_api = filter_panel_api
  )

  card$append_text("Selected Options", "header3")
  card$append_text(
    paste(
      formatted_data_constraint(
        constraint_list$constraint_var,
        constraint_list$constraint_range_min,
        constraint_list$constraint_range_max
      ),
      constraint_description
    ),
    style = style
  )
  card
}

#' Get Choices
#'
#' This function returns choices based on the class of the input.
#' If the input is of class `delayed_data`, it returns the `subset` of the input.
#' If `subset` is NULL and the input contains `var_label` and `var_choices`,
#' it throws an error prompting to resolve delayed inputs.
#' Otherwise, it returns the input as is.
#'
#' @param choices An object that contains choices.
#' @return A vector of choices.
#' @keywords internal
get_choices <- function(choices) {
  if (inherits(choices, "delayed_data")) {
    if (is.null(choices$subset)) {
      if (!is.null(choices$var_label) && !is.null(choices$var_choices)) {
        stop(
          "Resolve delayed inputs by evaluating the code within the provided datasets.
          Check ?teal.transform::resolve_delayed for more information."
        )
      } else {
        stop("Subset is NULL and necessary fields are missing.")
      }
    } else {
      choices$subset
    }
  } else {
    choices
  }
}
