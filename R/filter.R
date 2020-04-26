#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are `TRUE`.
#'
#' @param .data A `data.frame`.
#' @param ... Logical predicated defined in terms of the variables in `.data`. Multiple conditions are combined with
#' `&`. Arguments within `...` are automatically quoted and evaluated within the context of the `data.frame`.
#'
#' @section Useful filter functions:
#'
#' * `==`, `>`, `>=`, etc.
#' * `&`, `|`, `!`, `xor()`
#' * `is.na()`
#'
#' @examples
#' filter(mtcars, am == 1)
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#'
#' @return
#' A `data.frame`.
#'
#' @export
filter <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("filter")
}

#' @export
filter.default <- function(.data, ...) {
  conditions <- paste(deparse_dots(...), collapse = " & ")
  context$.data <- .data
  on.exit(rm(.data, envir = context))
  .data[do.call(with, list(.data, str2lang(unname(conditions)))), ]
}

#' @export
filter.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "filter", ...)
}
