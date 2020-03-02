#' @examples
#' rename(mtcars, MilesPerGallon = mpg)
#' rename(mtcars, Cylinders = cyl, Gears = gear)
#' mtcars %>% rename(MilesPerGallon = mpg)
#'
#' @rdname select
#' @export
rename <- function(.data, ...) {
  check_is_dataframe(.data)
  map <- deparse_dots(...)
  new_names <- names(map)
  if (length(new_names) == 0L) {
    warning("You didn't give any new names")
    return(.data)
  }
  col_names <- colnames(.data)
  no_new_name <- which(nchar(new_names) == 0L)
  old_names <- unname(map)
  if (length(no_new_name) > 0L) {
    warning("You didn't provide new names for: ", paste0("`", extract(map, no_new_name), collapse = ", "), "`")
    new_names <- extract(new_names, -no_new_name)
    old_names <- extract(old_names, -no_new_name)
  }
  set_colnames(.data, inset(col_names, which(col_names %in% old_names), new_names))
}
