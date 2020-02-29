#' Mutate
#'
#' @param .data A `data.frame`.
#' @param ... Expressions to mutate the data by.
#'
#' @examples
#' mutate(mtcars, mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2, cyl2 = cyl * 2)
#'
#' @export
mutate <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("mutate")
}

#' @export
mutate.default <- function(.data, ...) {
  conditions <- deparse_dots(...)
  new_data <- lapply(
    conditions,
    function(x, .data) with(.data, eval(parse(text = x))),
    .data
  )
  inset(.data, , names(conditions), new_data)
}

#' @export
mutate.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "mutate", ...)
}
