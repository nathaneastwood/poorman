#' Select/rename variables by name
#'
#' Choose or rename variables from a `data.frame`. `select()` keeps only the
#' variables you mention; `rename()` keeps all the variables.
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @name select
#' @return A `data.frame`.
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
  map <- names(deparse_dots(...))
  col_pos <- select_positions(.data, ...)
  res <- extract(.data, , col_pos, drop = FALSE)
  to_map <- nchar(map) > 0L
  colnames(res)[to_map] <- map[to_map]
  res
}
