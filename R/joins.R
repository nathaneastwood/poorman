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
#' @param by A character vector of variables to join by. If `NULL`, the default, `*_join()` will do a natural join,
#' using all variables with common names across the two tables. A message lists the variables so that you can check
#' they're right (to suppress the message, simply explicitly list the variables that you want to join).
#'
#' To join by different variables on x and y use a named vector. For example, `by = c("a" = "b")` will match `x.a` to
#' `y.b`.
#'
#' To join by multiple variables, use a vector with length > 1. For example, `by = c("a", "b")` will match `x$a` to
#' `y$a` and `x$b` to `y$b`. Use a named vector to match different variables in `x` and `y`. For example,
#' `by = c("a" = "b", "c" = "d")` will match `x$a` to `y$b` and `x$c` to `y$d`.
#'
#' To perform a cross-join, generating all combinations of `x` and `y`, use `by = character()`.
#' @param suffix `character(2)`. If there are non-joined duplicate variables in `x` and `y`, these suffixes will be
#' added to the output to disambiguate them.
#' @param ... Additional arguments to pass to [merge()]
#' @param keep `logical(1)`. Should the join keys from both `x` and `y` be preserved in the output? Only applies to
#' `left_join()`, `right_join()`, and `full_join()`.
#' @param na_matches Should `NA` and `NaN` values match one another?
#'
#' The default, `"na"`, treats two `NA` or `NaN` values as equal, like `%in%`, [match()], [merge()].
#'
#' Use `"never"` to always treat two `NA` or `NaN` values as different, like joins for database sources, similarly to
#' `merge(incomparables = FALSE)`.
#'
#' @return
#' A `data.frame`. The order of the rows and columns of `x` is preserved as much as possible. The output has the
#' following properties:
#'
#' * For `inner_join()`, a subset of `x` rows.
#'   For `left_join()`, all `x` rows.
#'   For `right_join()`, a subset of `x` rows, followed by unmatched `y` rows.
#'   For `full_join()`, all `x` rows, followed by unmatched `y` rows.
#' * For all joins, rows will be duplicated if one or more rows in `x` matches multiple rows in `y`.
#' * Output columns include all `x` columns and all `y` columns. If columns in `x` and `y` have the same name (and
#'   aren't included in `by`), `suffix`es are added to disambiguate.
#' * Output columns included in `by` are coerced to common type across `x` and `y`.
#' * Groups are taken from `x`.
#'
#' @examples
#' # If a row in `x` matches multiple rows in `y`, all the rows in `y` will be
#' # returned once for each matching row in `x`
#' df1 <- data.frame(x = 1:3)
#' df2 <- data.frame(x = c(1, 1, 2), y = c("first", "second", "third"))
#' df1 %>% left_join(df2)
#'
#' # By default, NAs match other NAs so that there are two
#' # rows in the output of this join:
#' df1 <- data.frame(x = c(1, NA), y = 2)
#' df2 <- data.frame(x = c(1, NA), z = 3)
#' left_join(df1, df2)
#'
#' # You can optionally request that NAs don't match, giving a
#' # a result that more closely resembles SQL joins
#' left_join(df1, df2, na_matches = "never")
#'
#' @name mutate_joins
NULL

#' @rdname mutate_joins
#' @export
inner_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., na_matches = c("na", "never")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, sort = FALSE, ..., keep = FALSE, na_matches = na_matches)
}

#' @rdname mutate_joins
#' @export
left_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all.x = TRUE, ..., keep = keep, na_matches = na_matches)
}

#' @rdname mutate_joins
#' @export
right_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all.y = TRUE, ..., keep = keep, na_matches = na_matches)
}

#' @rdname mutate_joins
#' @export
full_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")) {
  join_worker(x = x, y = y, by = by, suffix = suffix, all = TRUE, ..., keep = keep, na_matches = na_matches)
}

join_worker <- function(x, y, by = NULL, suffix = c(".x", ".y"), keep = FALSE, na_matches = c("na", "never"), ...) {
  na_matches <- match.arg(arg = na_matches, choices = c("na", "never"), several.ok = FALSE)
  incomparables <- if (na_matches == "never") NA else NULL
  x[, ".join_id"] <- seq_len(nrow(x))
  merged <- if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
    merge(
      x = x, y = y, by = by, suffixes = suffix, incomparables = incomparables, ...
    )[, union(names(x), names(y)), drop = FALSE]
  } else if (is.null(names(by))) {
    merge(x = x, y = y, by = by, suffixes = suffix, incomparables = incomparables, ...)
  } else {
    merge(x = x, y = y, by.x = names(by), by.y = by, suffixes = suffix, incomparables = incomparables, ...)
  }
  merged <- merged[order(merged[, ".join_id"]), colnames(merged) != ".join_id", drop = FALSE]
  if (isTRUE(keep)) {
    keep_pos <- match(by, names(merged))
    x_by <- paste0(by, suffix[1L])
    colnames(merged)[keep_pos] <- x_by
    merged[, paste0(by, suffix[2L])] <- merged[, x_by]
  }
  rownames(merged) <- NULL
  reconstruct_attrs(merged, x)
}

join_message <- function(by) {
  if (length(by) > 1L) {
    message("Joining, by = c(\"", paste0(by, collapse = "\", \""), "\")\n", sep = "")
  } else {
    message("Joining, by = \"", by, "\"\n", sep = "")
  }
}
