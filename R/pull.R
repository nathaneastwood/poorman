#' Pull out a single variable
#'
#' This is a direct replacement for `[[.data.frame`.
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
  var_deparse <- deparse(substitute(var))
  col_names <- colnames(.data)
  if (!(var_deparse %in% col_names) & grepl("^[[:digit:]]+L|[[:digit:]]", var_deparse)) {
    var <- as.integer(gsub("L", "", var_deparse))
    var <- ifelse(var < 1L, rev(col_names)[abs(var)], col_names[var])
  } else if (var_deparse %in% col_names) {
    var <- var_deparse
  }
  .data[, var]
}
