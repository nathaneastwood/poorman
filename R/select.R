#' Select
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @examples
#' select(mtcars, mpg, cyl)
#' \dontrun{
#' mtcars %>% select(mpg)
#' mtcars %>% select(mpg, cyl)
#' }
#'
#' @export
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
#' \dontrun{
#' mtcars %>% pull(mpg)
#' }
#'
#' @export
pull <- function(.data, var) {
  var <- deparse(substitute(var))
  extract2(.data, var)
}
