#' Filter
#'
#' @param .data A `data.frame`.
#' @param ... Expressions used to filter the data by.
#'
#' @examples
#' filter(mtcars, am == 1)
#' \dontrun{
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#' }
#'
#' @export
filter <- function(.data, ...) {
  conditions <- paste(vapply(substitute(...()), deparse, NA_character_), collapse = " & ")
  extract(.data, with(.data, eval(parse(text = conditions))), )
}

#' Slice
#'
#' @param .data A `data.frame`.
#' @param ... A `vector()` of rows to subset.
#'
#' @examples
#' slice(mtcars, c(1, 2, 3))
#' \dontrun{
#' mtcars %>% slice(1:3)
#' }
#'
#' @export
slice <- function(.data, ...) {
  stopifnot(is.numeric(...) || is.integer(...))
  extract(.data, ..., )
}
