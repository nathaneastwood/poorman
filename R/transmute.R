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
  rows <- rownames(.data)
  res <- apply_grouped_function("transmute", .data, drop = TRUE, ...)
  res[rows, ]
}
