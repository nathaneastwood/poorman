#' Select
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @examples
#' select(mtcars, mpg, cyl)
#' select(mtcars, MilesPerGallon = mpg, Cylinders = cyl)
#' mtcars %>% select(mpg)
#' mtcars %>% select(mpg, cyl)
#'
#' @name select
#' @export
select <- function(.data, ...) {
  check_is_dataframe(.data)
  cols <- deparse_dots(...)
  map <- names(cols)
  if (!is.null(map)) .data <- rename(.data, ...)
  cols <- if (is.null(map)) cols else map
  extract(.data, , cols, drop = FALSE)
}
