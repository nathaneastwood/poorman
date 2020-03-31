#' Choose rows by position
#'
#' Choose rows by their original position in the `data.frame`. Grouped `data.frame`s use the position within each group.
#'
#' @param .data A `data.frame`.
#' @param ... Integer row values. Provide either positive values to keep, or negative values to drop. The values
#' provided must be either all positive or negative. Indices beyond the number of rows in the input are silently
#' ignored.
#'
#' @examples
#' slice(mtcars, c(1, 2, 3))
#' mtcars %>% slice(1:3)
#'
#' @export
slice <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("slice")
}

#' @export
slice.default <- function(.data, ...) {
  rows <- c(...)
  stopifnot(is.numeric(rows) | is.integer(rows))
  if (all(rows > 0L)) {
    max_rows <- nrow(.data)
    rows <- intersect(rows, seq_len(max_rows))
  }
  extract(.data, rows, )
}

#' @export
slice.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "slice", ...)
}
