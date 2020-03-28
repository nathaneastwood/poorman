#' Change column order
#'
#' Change the column positions of a `data.frame`.
#'
#' @param .data A `data.frame`.
#' @param ... The columns to move.
#' @param .before,.after Destination of the columns selected by `...`. Supplying neither will move the columns to the
#' left-hand side whereas supplying both will result in an error.
#'
#' @return
#' A `data.frame`
#'
#' @export
relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  check_is_dataframe(.data)
  data_names <- colnames(.data)
  col_pos <- select_positions(.data, ...)

  .before <- if (missing(.before)) .before else deparse(substitute(.before))
  .after <- if (missing(.after)) .after else deparse(substitute(.after))
  has_before <- !is.null(.before)
  has_after <- !is.null(.after)

  if (has_before && has_after) {
    stop("You must supply only one of `.before` and `.after`")
  } else if (has_before) {
    where <- min(match(.before, data_names))
    col_pos <- c(setdiff(col_pos, where), where)
  } else if (has_after) {
    where <- max(match(.after, data_names))
    col_pos <- c(where, setdiff(col_pos, where))
  } else {
    where <- 1L
    col_pos <- union(col_pos, where)
  }

  lhs <- setdiff(seq(1L, where - 1L), col_pos)
  rhs <- setdiff(seq(where + 1L, ncol(.data)), col_pos)

  .data[unique(c(lhs, col_pos, rhs))]
}
