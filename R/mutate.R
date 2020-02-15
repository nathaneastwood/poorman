#' Mutate
#'
#' @param .data A `data.frame`.
#' @param ... Expressions to mutate the data by.
#'
#' @examples
#' mutate(mtcars, mpg2 = mpg * 2)
#' \dontrun{
#' mtcars %>% mutate(mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2, cyl2 = cyl * 2)
#' }
#'
#' @export
mutate <- function(.data, ...) {
  conditions <- vapply(substitute(...()), deparse, NA_character_)
  new_data <- lapply(
    conditions,
    function(x, .data) with(.data, eval(parse(text = x))),
    .data
  )
  inset(.data, , names(conditions), new_data)
}

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
  conditions <- vapply(substitute(...()), deparse, NA_character_)
  mutated <- mutate(.data, ...)
  extract(mutated, names(conditions))
}
