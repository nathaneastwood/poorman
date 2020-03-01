#' Select/rename variables by name
#'
#' Choose or rename variables from a `data.frame`. `select()` keeps only the
#' variables you mention; `rename()` keeps all the variables.
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @name select
NULL

#' @examples
#' select(mtcars, mpg, cyl)
#' select(mtcars, MilesPerGallon = mpg, Cylinders = cyl)
#' mtcars %>% select(mpg)
#' mtcars %>% select(mpg, cyl)
#'
#' @rdname select
#' @export
select <- function(.data, ...) {
  check_is_dataframe(.data)
  cols <- deparse_dots(...)
  map <- names(cols)
  if (!is.null(map)) .data <- rename(.data, ...)
  cols <- if (is.null(map)) cols else map
  extract(.data, , cols, drop = FALSE)
}
