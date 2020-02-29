#' Filter
#'
#' @param .data A `data.frame`.
#' @param ... Expressions used to filter the data by.
#'
#' @examples
#' filter(mtcars, am == 1)
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#'
#' @export
filter <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("filter")
}

#' @export
filter.default <- function(.data, ...) {
  conditions <- paste(deparse_dots(...), collapse = " & ")
  extract(.data, with(.data, eval(parse(text = conditions))), )
}

#' @export
filter.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "filter", ...)
}

#' Slice
#'
#' @param .data A `data.frame`.
#' @param ... A `vector()` of rows to subset.
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
