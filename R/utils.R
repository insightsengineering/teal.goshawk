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

#' Set the attributes of the last chunk outputs
#'
#' @param teal_card (\code{teal_card}) object to modify.
#' @param attributes (\code{list}) of attributes to set on the last chunk outputs.
#' @param n (\code{integer(1)}) number of the last element of \code{teal_card} to modify.
#' it will only change \code{chunk_output} objects.
#' @param inner_classes (\code{character}) classes within \code{chunk_output} that should be modified.
#' This can be used to only change \code{recordedplot}, \code{ggplot2} or other type of objects.
#' @param quiet (\code{logical(1)}) whether to suppress warnings.
#'
#' @return The modified \code{teal_card} object
#'
#' @keywords internal
set_chunk_attrs <- function(teal_card,
                            attributes,
                            n = 1,
                            inner_classes = NULL,
                            quiet = FALSE) {
  checkmate::assert_class(teal_card, "teal_card")
  checkmate::assert_list(attributes, names = "unique")
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_character(inner_classes, null.ok = TRUE)
  checkmate::assert_flag(quiet)

  if (!inherits(teal_card[[length(teal_card)]], "chunk_output")) {
    if (!quiet) {
      warning("The last element of the `teal_card` is not a `chunk_output` object. No attributes were modified.")
    }
    return(teal_card)
  }

  for (ix in seq_len(length(teal_card))) {
    if (ix > n) {
      break
    }
    current_ix <- length(teal_card) + 1 - ix
    if (!inherits(teal_card[[current_ix]], "chunk_output")) {
      if (!quiet) {
        warning(
          "The ", ix,
          " to last element of the `teal_card` is not a `chunk_output` object. Skipping any further modifications."
        )
      }
      return(teal_card)
    }

    if (length(inner_classes) > 0 && !checkmate::test_multi_class(teal_card[[current_ix]][[1]], inner_classes)) {
      next
    }

    attributes(teal_card[[current_ix]]) <- utils::modifyList(
      attributes(teal_card[[current_ix]]),
      attributes
    )
  }

  teal_card
}

#' Create a reactive that sets plot dimensions on a \code{teal_card}
#'
#' This is a convenience function that creates a reactive expression that
#' automatically sets the \code{dev.width} and \code{dev.height} attributes on the last
#' chunk outputs of a \code{teal_card} based on plot dimensions from a plot widget.
#'
#' @param pws (\code{plot_widget}) plot widget that provides dimensions via \code{dim()} method
#' @param decorated_output_q (\code{reactive}) reactive expression that returns a \code{teal_card}
#' @param inner_classes (\code{character}) classes within \code{chunk_output} that should be modified.
#' This can be used to only change \code{recordedplot}, \code{ggplot2} or other type of objects.
#'
#' @return A reactive expression that returns the \code{teal_card} with updated dimensions
#'
#' @keywords internal
set_chunk_dims <- function(pws, decorated_output_q, inner_classes = NULL) {
  checkmate::assert_class(pws, "plot_widget")
  checkmate::assert_class(decorated_output_q, "reactive")
  checkmate::assert_character(inner_classes, null.ok = TRUE)

  reactive({
    dims <- req(pws$dim())
    q <- req(decorated_output_q())
    teal.reporter::teal_card(q) <- set_chunk_attrs(
      teal.reporter::teal_card(q),
      list(dev.width = dims[[1]], dev.height = dims[[2]]),
      inner_classes = inner_classes
    )
    q
  })
}
