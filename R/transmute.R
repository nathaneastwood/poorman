#' Transmute
#'
#' @param .data A `data.frame`.
#' @param ... Expressions to mutate the data by.
#'
#' @examples
#' transmute(mtcars, mpg2 = mpg * 2)
#' \dontrun{
#' mtcars %>% transmute(mpg2 = mpg * 2, cyl2 = cyl * 2)
#' }
#'
#' @export
transmute <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("transmute")
}

#' @export
transmute.default <- function(.data, ...) {
  conditions <- deparse_dots(...)
  mutated <- mutate(.data, ...)
  extract(mutated, names(conditions))
}

#' @export
transmute.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "transmute", ...)
}
