#' Change column order
#'
#' Use `relocate()` to change column positions, using the same syntax as [select()] to make it easy to move blocks of
#' columns at once.
#'
#' @inheritParams select
#' @param ... <[`poor-select`][select_helpers]> Columns to move.
#' @param .before,.after <[`poor-select`][select_helpers]> Destination of columns selected by `...`. Supplying neither
#' will move columns to the left-hand side; specifying both will result in an error.
#'
#' @return
#' An object of the same type as `.data`. The output has the following properties:
#'
#' * Rows are not affected.
#' * The same columns appear in the output, but (usually) in a different place.
#' * Data frame attributes are preserved.
#' * Groups are not affected.
#'
#' @examples
#' df <- data.frame(
#'   a = 1, b = 1, c = 1, d = "a", e = "a", f = "a",
#'   stringsAsFactors = FALSE
#' )
#' df %>% relocate(f)
#' df %>% relocate(a, .after = c)
#' df %>% relocate(f, .before = b)
#' df %>% relocate(a, .after = last_col())
#'
#' # Can also select variables based on their type
#' df %>% relocate(where(is.character))
#' df %>% relocate(where(is.numeric), .after = last_col())
#' # Or with any other select helper
#' df %>% relocate(any_of(c("a", "e", "i", "o", "u")))
#'
#' # When .before or .after refers to multiple variables they will be
#' # moved to be immediately before/after the selected variables.
#' df2 <- data.frame(
#'   a = 1, b = "a", c = 1, d = "a",
#'   stringsAsFactors = FALSE
#' )
#' df2 %>% relocate(where(is.numeric), .after = where(is.character))
#' df2 %>% relocate(where(is.numeric), .before = where(is.character))
#'
#' @export
relocate <- function(.data, ..., .before = NULL, .after = NULL) {
  UseMethod("relocate")
}

#' @export
relocate.data.frame <- function(.data, ..., .before = NULL, .after = NULL) {
  data_names <- colnames(.data)
  col_pos <- select_positions(.data, ...)

  if (!missing(.before)) {
    x <- try(eval(.before), silent = TRUE)
    if (inherits(x, "try-error")) {
      .before <- colnames(.data)[eval_select_pos(.data, substitute(.before))]
    } else if (is.null(x)) {
      .after <- NULL
    } else {
      .before <- colnames(.data)[eval_select_pos(.data, .before)]
    }
  }
  if (!missing(.after)) {
    x <- try(eval(.after), silent = TRUE)
    if (inherits(x, "try-error")) {
      .after <- colnames(.data)[eval_select_pos(.data, substitute(.after))]
    } else if (is.null(x)) {
      .after <- NULL
    } else {
      .after <- colnames(.data)[eval_select_pos(.data, .after)]
    }
  }

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
  if (has_groups(.data)) res <- groups_set(res, group_vars(.data))
  res
}
