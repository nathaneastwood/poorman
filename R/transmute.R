#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  # `mutate()` evaluates in the frame which calls it, which here is this function, so pass on the frame of the user
  # unless a grouped method has already done so
  if (is.null(context$group_env)) {
    context$group_env <- parent.frame()
    on.exit(context$group_env <- NULL, add = TRUE)
  }
  mutate(.data, ..., .keep = "none")
}

#' @export
transmute.grouped_df <- function(.data, ...) {
  context$group_env <- parent.frame()
  on.exit(context$group_env <- NULL, add = TRUE)
  apply_grouped_function(.data, function(df) transmute(df, ...), .drop = TRUE, .reorder = TRUE)
}
