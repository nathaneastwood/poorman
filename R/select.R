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
  cols <- gsub('[\"]', '', deparse_dots(...))
  map <- names(cols)
  col_nums <- suppressWarnings(as.integer(cols))
  char_cols <- which(colnames(.data) %in% cols[which(is.na(col_nums))])
  col_nums[is.na(col_nums)] <- char_cols
  res <- extract(.data, , col_nums, drop = FALSE)
  to_map <- nchar(map) > 0L
  colnames(res)[to_map] <- map[to_map]
  res
}
