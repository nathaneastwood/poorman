#' @examples
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("transmute")
}

#' @export
transmute.default <- function(.data, ...) {
  conditions <- deparse_dots(...)
  mutated <- mutate(.data, ...)
  extract(mutated, names(conditions))
}

#' @export
transmute.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "transmute", ...)
}
