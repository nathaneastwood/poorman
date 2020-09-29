#' Mutating Joins
#'
#' @description
#' The mutating joins add columns from `y` to `x`, matching rows based on the keys:
#'
#' * `inner_join()`: includes all rows in `x` and `y`.
#' * `left_join()`: includes all rows in `x`.
#' * `right_join()`: includes all rows in `y`.
#' * `full_join()`: includes all rows in `x` or `y`.
#'
#' If a row in `x` matches multiple rows in `y`, all the rows in `y` will be returned once for each matching row in `x`.
#'
#' @param x,y The `data.frame`s to join.
#' @param by A character vector of variables to join by. If `NULL`, the default,
#' `*_join()` will do a natural join, using all variables with common names
#' across the two tables. A message lists the variables so that you can check
#' they're right (to suppress the message, simply explicitly list the variables
#' that you want to join).
#'
#' To join by different variables on x and y use a named vector. For example,
#' `by = c("a" = "b")` will match `x.a` to `y.b`.
#' @param suffix If there are non-joined duplicate variables in `x` and `y`,
#' these suffixes will be added to the output to disambiguate them. Should be a
#' character vector of length 2.
#'
#' @name mutate_joins
NULL

#' @rdname mutate_joins
#' @export
inner_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, sort = FALSE)
}

#' @rdname mutate_joins
#' @export
left_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all.x = TRUE)
}

#' @rdname mutate_joins
#' @export
right_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all.y = TRUE)
}

#' @rdname mutate_joins
#' @export
full_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all = TRUE)
}

join_worker <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...) {
  x[, ".join_id"] <- seq_len(nrow(x))
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
    merged <- merge(x = x, y = y, by = by, suffixes = suffix, ...)[, union(names(x), names(y))]
  } else if (is.null(names(by))) {
    merged <- merge(x = x, y = y, by = by, suffixes = suffix, ...)
  } else {
    merged <- merge(x = x, y = y, by.x = names(by), by.y = by, suffixes = suffix, ...)
  }
  merged <- merged[order(merged[, ".join_id"]), colnames(merged) != ".join_id"]
  rownames(merged) <- NULL
  merged
}

join_message <- function(by) {
  if (length(by) > 1L) {
    message("Joining, by = c(\"", paste0(by, collapse = "\", \""), "\")\n", sep = "")
  } else {
    message("Joining, by = \"", by, "\"\n", sep = "")
  }
}
