#' Rename columns
#'
#' `rename()` changes the names of individual variables using `new_name = old_name` syntax.
#'
#' @param .data A `data.frame`
#' @param ... Comma separated key-value pairs in the form of `new_name = old_name` to rename selected variables.
#'
#' @return A `data.frame`
#'
#' @examples
#' rename(mtcars, MilesPerGallon = mpg)
#' rename(mtcars, Cylinders = cyl, Gears = gear)
#' mtcars %>% rename(MilesPerGallon = mpg)
#'
#' @export
rename <- function(.data, ...) {
  check_is_dataframe(.data)
  new_names <- names(deparse_dots(...))
  if (length(new_names) == 0L) {
    warning("You didn't give any new names")
    return(.data)
  }
  col_pos <- select_positions(.data, ...)
  old_names <- colnames(.data)[col_pos]
  new_names_zero <- nchar(new_names) == 0L
  if (any(new_names_zero)) {
    warning("You didn't provide new names for: ", paste0("`", old_names[new_names_zero], collapse = ", "), "`")
    new_names[new_names_zero] <- old_names[new_names_zero]
  }
  colnames(.data)[col_pos] <- new_names
  .data
}
