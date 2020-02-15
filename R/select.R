#' Select
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @examples
#' select(mtcars, mpg, cyl)
#' mtcars %>% select(mpg)
#' mtcars %>% select(mpg, cyl)
select <- function(.data, ...) {
  cols <- vapply(substitute(...()), deparse, NA_character_)
  extract(.data, , cols, drop = FALSE)
}

#' Pull
#'
#' @param .data A `data.frame`.
#' @param ... The name of the column to extract
#'
#' @examples
#' pull(mtcars, mpg)
#' mtcars %>% pull(mpg)
pull <- function(.data, var) {
  var <- deparse(substitute(var))
  stopifnot(length(var) == 1)
  extract(.data, , var, drop = TRUE)
}
