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
  cols <- deparse_dots(...)
  map <- names(cols)
  if (!is.null(map)) .data <- rename(.data, ...)
  cols <- if (is.null(map)) cols else map
  extract(.data, , cols, drop = FALSE)
}

#' Pull out a single variable
#'
#' @param .data A `data.frame`.
#' @param var A variable specified as:
#' * a literal variable name
#' * a positive integer, giving the position counting from the left
#' * a negative integer, giving the position counting from the right
#'
#' The default returns the last column (on the assumption that's the column you've created most recently).
#'
#' @examples
#' mtcars %>% pull(-1)
#' mtcars %>% pull(1)
#' mtcars %>% pull(cyl)
#'
#' @export
pull <- function(.data, var = -1) {
  var <- deparse(substitute(var))
  if (!(var %in% colnames(.data)) & grepl("^[[:digit:]]+L|[[:digit:]]", var)) {
    var <- as.integer(gsub("L", "", var))
    var <- ifelse(var < 1L, extract(rev(colnames(.data)), abs(var)), extract(colnames(.data), var))
  }
  extract2(.data, var)
}
