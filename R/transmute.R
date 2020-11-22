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
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  conditions <- dotdotdot(...)
  mutated <- mutate(.data, ...)
  mutated[, names(conditions), drop = FALSE]
}

#' @export
transmute.grouped_data <- function(.data, ...) {
  rows <- rownames(.data)
  res <- apply_grouped_function("transmute", .data, drop = TRUE, ...)
  res[rows, ]
}
