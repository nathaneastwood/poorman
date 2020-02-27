#' Rename
#'
#' @examples
#' rename(mtcars, MilesPerGallon = mpg)
#' rename(mtcars, Cylinders = cyl, Gears = gear)
#' mtcars %>% rename(MilesPerGallon = mpg)
#' @rdname select
#' @export
rename <- function(.data, ...) {
  map <- deparse_dots(...)
  col_names <- colnames(.data)
  set_colnames(.data, inset(col_names, which(col_names %in% unname(map)), names(map)))
}
