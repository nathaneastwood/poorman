#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones; `transmute()` adds new variables and drops existing ones.
#' Both functions preserve the number of rows of the input. New variables overwrite existing variables of the same name.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of expressions, each with length `1L`. The name of each argument will be the name of a
#' new column and the value will be its corresponding value. Use a `NULL` value in ‘mutate’ to drop a variable. New
#' variables overwrite existing variables of the same name.
#'
#' @examples
#' mutate(mtcars, mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2, cyl2 = cyl * 2)
#'
#' @name mutate
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
