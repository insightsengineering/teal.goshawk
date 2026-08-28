#' Set the attributes of the last chunk outputs
#' @param teal_card (`teal_card`) object to modify.
#' @param attributes (`list`) of attributes to set on the last chunk outputs.
#' @param n (`integer(1)`) number of the last element of `teal_card` to modify.
#' it will only change `chunk_output` objects.
#' @param inner_classes (`character`) classes within `chunk_output` that should be modified.
#' This can be used to only change `recordedplot`, `ggplot2` or other type of objects.
#'
#' @return The modified `teal_card` object with updated attributes for the last chunk outputs.
#' @keywords internal
#' @examples
#' set_chunk_attrs <- getFromNamespace("set_chunk_attrs", "teal.goshawk")
#' td <- within(
#'   teal.reporter::teal_report(),
#'   ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
#' )
#' card <- teal.reporter::teal_card(td)
#' set_chunk_attrs(card, list(dev.width = 200, dev.height = 100))
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

  for (ix in seq_along(teal_card)) {
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

    if (
      length(inner_classes) > 0 &&
        length(teal_card[[current_ix]]) >= 1 &&
        !checkmate::test_multi_class(teal_card[[current_ix]][[1]], inner_classes)
    ) {
      next
    }

    attributes(teal_card[[current_ix]]) <- utils::modifyList(
      attributes(teal_card[[current_ix]]),
      attributes
    )
  }

  teal_card
}

#' Create a reactive that sets plot dimensions on a `teal_card`
#'
#' This is a convenience function that creates a reactive expression that
#' automatically sets the `dev.width` and `dev.height` attributes on the last
#' chunk outputs of a `teal_card` based on plot dimensions from a plot widget.
#'
#' @param pws (`plot_widget`) plot widget that provides dimensions via `dim()` method
#' @param q_r (`reactive`) reactive expression that returns a `teal_reporter`
#' @param inner_classes (`character`) classes within `chunk_output` that should be modified.
#' This can be used to only change `recordedplot`, `ggplot2` or other type of objects.
#'
#' @return A reactive expression that returns the `teal_card` with updated dimensions
#'
#' @keywords internal
#'
#' @examples
#' set_chunk_dims <- getFromNamespace("set_chunk_dims", "teal.goshawk")
#' td <- within(
#'   teal.reporter::teal_report(),
#'   ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
#' )
#' pws <- list(dim = reactive({ list(width = 200, height = 100) })) # mocking a teal.widget::plot_with_settings
#' set_chunk_dims(pws = pws, q_r = reactive(td))
set_chunk_dims <- function(pws, q_r, inner_classes = NULL) {
  checkmate::assert_list(pws)
  checkmate::assert_names(names(pws), must.include = "dim")
  checkmate::assert_class(pws$dim, "reactive")
  checkmate::assert_class(q_r, "reactive")
  checkmate::assert_character(inner_classes, null.ok = TRUE)

  reactive({
    pws_dim <- stats::setNames(as.list(req(pws$dim())), c("width", "height"))
    if (identical(pws_dim$width, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$width <- NULL
    }
    if (identical(pws_dim$height, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$height <- NULL
    }
    q <- req(q_r())
    teal.reporter::teal_card(q) <- set_chunk_attrs(
      teal.reporter::teal_card(q),
      list(dev.width = pws_dim$width, dev.height = pws_dim$height),
      inner_classes = inner_classes
    )
    q
  })
}
