#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  mutate(.data, ..., .keep = "none")
}

#' @export
transmute.grouped_df <- function(.data, ...) {
  apply_grouped_function(.fn = "transmute", .data = .data, .drop = TRUE, ..., .reorder = TRUE)
}
