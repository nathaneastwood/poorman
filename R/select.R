#' Select/relocate variables by name
#'
#' Choose or relocate variables from a `data.frame`. `select()` keeps only the
#' variables you mention; `relocate()` keeps all the variables.
#'
#' @param .data A `data.frame`.
#' @param ... The name(s) of the column(s) to select.
#'
#' @section Useful functions:
#' There are a number of special functions which are designed to work in `select()` and `relocate()`:
#' * [starts_with()], [ends_with()], [contains()]
#' * [matches()]
#' * [num_range()]
#' * [everything()]
#'
#' @return A `data.frame`.
#'
#' @examples
#' select(mtcars, mpg, cyl)
#' select(mtcars, MilesPerGallon = mpg, Cylinders = cyl)
#' mtcars %>% select(mpg)
#' mtcars %>% select(mpg, cyl)
#' iris %>% select(contains("Petal"))
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- as.data.frame(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' df %>% select(num_range("V", 4:6))
#'
#' mtcars %>% relocate(ends_with("p"), .before = mpg)
#'
#' @name select
NULL

#' @rdname select
#' @export
select <- function(.data, ...) {
  map <- names(deparse_dots(...))
  col_pos <- select_positions(.data, ..., group_pos = TRUE)
  res <- .data[, col_pos, drop = FALSE]
  to_map <- nchar(map) > 0L
  colnames(res)[to_map] <- map[to_map]
  if (has_groups(.data)) res <- set_groups(res, get_groups(.data))
  res
}
