#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  # `mutate()` evaluates in the frame which calls it, which here is this function, so pass on the frame of the user
  # unless an outer verb has already done so
  if (is.null(context$group_env)) {
    context$group_env <- parent.frame()
    on.exit(context$group_env <- NULL, add = TRUE)
  }
  mutate(.data, ..., .keep = "none")
}
