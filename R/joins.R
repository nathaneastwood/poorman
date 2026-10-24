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
  natural <- is.null(by)
  by <- standardise_join_by(by, names(x), names(y))
  # merge() does not cast the keys to a common type, so the key of an inner or left join would keep the type of `x`
  for (i in seq_along(by$x)) {
    cast <- join_key_cast(.subset2(x, by$x[i]), .subset2(y, by$y[i]), by$x[i], mutating = TRUE)
    x[[by$x[i]]] <- cast$x
    y[[by$y[i]]] <- cast$y
  }
  x[, ".join_id"] <- seq_len(nrow(x))
  merged <- merge(x = x, y = y, by.x = by$x, by.y = by$y, suffixes = suffix, incomparables = incomparables, ...)
  if (natural) merged <- merged[, union(names(x), names(y)), drop = FALSE]
  merged <- merged[order(merged[, ".join_id"]), colnames(merged) != ".join_id", drop = FALSE]
  if (isTRUE(keep)) {
    keep_pos <- match(by$y, names(merged))
    x_by <- paste0(by$y, suffix[1L])
    colnames(merged)[keep_pos] <- x_by
    merged[, paste0(by$y, suffix[2L])] <- merged[, x_by]
  }
  rownames(merged) <- NULL
  reconstruct_attrs(merged, x)
}

#' Find the key columns of a join
#'
#' @param by The `by` argument of a join: `NULL` for a natural join, otherwise a character vector whose names, where
#' given, are the keys in `x`.
#' @param x_names,y_names `character(n)`. The column names of `x` and `y`.
#' @return A `list` with `x` and `y`, the names of the key columns in `x` and `y`.
#' @noRd
standardise_join_by <- function(by, x_names, y_names) {
  if (is.null(by)) {
    by <- intersect(x_names, y_names)
    if (length(by) == 0L) {
      stop(
        "`by` must be supplied when `x` and `y` have no common variables.\n",
        "i use `by = character()` to perform a cross-join."
      )
    }
    join_message(by)
  }
  by_x <- names(by)
  if (is.null(by_x)) by_x <- by
  by_x[by_x == ""] <- by[by_x == ""]
  by_y <- unname(by)
  check_join_vars(by_x, x_names)
  check_join_vars(by_y, y_names)
  list(x = unname(by_x), y = by_y)
}

join_message <- function(by) {
  if (length(by) > 1L) {
    message("Joining, by = c(\"", paste0(by, collapse = "\", \""), "\")\n", sep = "")
  } else {
    message("Joining, by = \"", by, "\"\n", sep = "")
  }
}

check_join_vars <- function(vars, names) {
  missing <- setdiff(vars, names)
  if (length(missing) > 0L) {
    stop("Join columns must be present in data.\nx Problem with ", paste0("`", missing, "`", collapse = ", "), ".")
  }
}

#' Cast a pair of join keys to their common type
#'
#' Follows `vctrs::vec_ptype2()`, as `dplyr` does: factors with different levels combine to a factor of all levels, a
#' factor and a character vector combine to a character vector, numbers combine to the widest numeric type and a
#' `Date` combines with a `POSIXct` to a `POSIXct`. Any other combination of types is an error.
#'
#' @param x_col,y_col The key columns of `x` and `y`.
#' @param name `character(1)`. The name of the key in `x`, used in errors.
#' @param mutating `logical(1)`. Is the key for a mutating join? These report incompatible types differently.
#' @return A `list` with `x` and `y`, `x_col` and `y_col` cast to the common type, and `key`, the values of `x_col` and
#' then `y_col` in a form that `group_ids()` can compare.
#' @noRd
join_key_cast <- function(x_col, y_col, name, mutating = FALSE) {
  # An all `NA` logical vector has no type of its own and takes the type of the other key
  if (is_unspecified(x_col) && !is_unspecified(y_col)) x_col <- y_col[rep(NA_integer_, length(x_col))]
  if (is_unspecified(y_col)) y_col <- x_col[rep(NA_integer_, length(y_col))]

  x_kind <- join_key_kind(x_col)
  y_kind <- join_key_kind(y_col)
  if (x_kind != y_kind) {
    x_type <- join_key_type(x_col)
    y_type <- join_key_type(y_col)
    if (mutating) {
      stop(
        "Can't join on `x$", name, "` x `y$", name, "` because of incompatible types.\n",
        "i `x$", name, "` is of type <", x_type, ">.\n",
        "i `y$", name, "` is of type <", y_type, ">."
      )
    }
    stop("Can't combine `", name, "` <", x_type, "> and `", name, "` <", y_type, ">.")
  }
  switch(
    x_kind,
    character = join_key_cast_character(x_col, y_col),
    numeric = {
      type <- typeof(c(x_col[0L], y_col[0L]))
      if (typeof(x_col) != type) storage.mode(x_col) <- type
      if (typeof(y_col) != type) storage.mode(y_col) <- type
      list(x = x_col, y = y_col, key = c(unclass(x_col), unclass(y_col)))
    },
    datetime = join_key_cast_datetime(x_col, y_col),
    list(x = x_col, y = y_col, key = c(x_col, y_col))
  )
}

join_key_cast_character <- function(x_col, y_col) {
  if (is.factor(x_col) && is.factor(y_col)) {
    lvls <- union(levels(x_col), levels(y_col))
    if (!identical(lvls, levels(x_col))) {
      x_col <- factor(as.character(x_col), levels = lvls, ordered = is.ordered(x_col))
    }
    if (!identical(lvls, levels(y_col))) {
      y_col <- factor(as.character(y_col), levels = lvls, ordered = is.ordered(y_col))
    }
  } else {
    x_col <- as.character(x_col)
    y_col <- as.character(y_col)
  }
  list(x = x_col, y = y_col, key = c(as.character(x_col), as.character(y_col)))
}

join_key_cast_datetime <- function(x_col, y_col) {
  if (inherits(x_col, "POSIXlt")) x_col <- as.POSIXct(x_col)
  if (inherits(y_col, "POSIXlt")) y_col <- as.POSIXct(y_col)
  # A Date is cast to a POSIXct at midnight in the time zone of the other key, as in `vctrs::vec_cast()`
  if (inherits(x_col, "Date") && inherits(y_col, "POSIXct")) {
    x_col <- as.POSIXct(as.character(x_col), tz = join_key_tz(y_col))
  } else if (inherits(x_col, "POSIXct") && inherits(y_col, "Date")) {
    y_col <- as.POSIXct(as.character(y_col), tz = join_key_tz(x_col))
  }
  # Dates are days and date-times are seconds, so only compare the underlying numbers once both have the same class
  list(x = x_col, y = y_col, key = c(unclass(x_col), unclass(y_col)))
}

join_key_tz <- function(x) {
  tz <- attr(x, "tzone", exact = TRUE)
  if (is.null(tz)) "" else tz[1L]
}

join_key_kind <- function(x) {
  if (is.character(x) || is.factor(x)) return("character")
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("datetime")
  if (is.null(attr(x, "class")) && typeof(x) %in% c("logical", "integer", "double")) return("numeric")
  paste(class(x), collapse = "/")
}

join_key_type <- function(x) {
  if (is.ordered(x)) return("ordered")
  if (is.factor(x)) return("factor")
  if (inherits(x, "Date")) return("date")
  if (inherits(x, c("POSIXct", "POSIXlt"))) return("datetime")
  if (is.null(attr(x, "class"))) return(typeof(x))
  class(x)[1L]
}
