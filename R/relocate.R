#' @param .before,.after Destination of the columns selected by `...`. Supplying neither will move the columns to the
#' left-hand side whereas supplying both will result in an error.
#'
#' @rdname select
#'
#' @export
relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  check_is_dataframe(.data)
  data_names <- colnames(.data)
  col_pos <- select_positions(.data, ...)

  .before <- deparse_var(.before)
  if (!is.null(.before)) .before <- colnames(.data)[eval_select_pos(.data, .before)]
  .after <- deparse_var(.after)
  if (!is.null(.after)) .after <- colnames(.data)[eval_select_pos(.data, .after)]

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
  col_pos <- unique(c(lhs, col_pos, rhs))
  col_pos <- col_pos[col_pos <= length(data_names)]

  res <- .data[col_pos]
  if (has_groups(.data)) res <- set_groups(res, get_groups(.data))
  res
}
