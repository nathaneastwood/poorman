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
#' @param copy Ignored. Included for compatibility with `dplyr`, where it allows `y` to come from a different data
#' source.
#' @param suffix `character(2)`. If there are non-joined duplicate variables in `x` and `y`, these suffixes will be
#' added to the output to disambiguate them.
#' @param ... Ignored. Included for compatibility with `dplyr`.
#' @param keep `logical(1)`. Should the join keys from both `x` and `y` be preserved in the output?
#' @param na_matches Should `NA` and `NaN` values match one another?
#'
#' The default, `"na"`, treats two `NA` or two `NaN` values as equal, like `%in%` and [match()].
#'
#' Use `"never"` to always treat two `NA` or `NaN` values as different, like joins for database sources.
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
inner_join <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")
) {
  join_worker(x = x, y = y, by = by, suffix = suffix, keep = keep, na_matches = na_matches, type = "inner")
}

#' @rdname mutate_joins
#' @export
left_join <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")
) {
  join_worker(x = x, y = y, by = by, suffix = suffix, keep = keep, na_matches = na_matches, type = "left")
}

#' @rdname mutate_joins
#' @export
right_join <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")
) {
  join_worker(x = x, y = y, by = by, suffix = suffix, keep = keep, na_matches = na_matches, type = "right")
}

#' @rdname mutate_joins
#' @export
full_join <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = FALSE, na_matches = c("na", "never")
) {
  join_worker(x = x, y = y, by = by, suffix = suffix, keep = keep, na_matches = na_matches, type = "full")
}

join_worker <- function(x, y, by, suffix, keep, na_matches, type) {
  na_matches <- match.arg(arg = na_matches, choices = c("na", "never"), several.ok = FALSE)
  keep <- isTRUE(keep)
  by <- join_key_names(by, names(x), names(y))
  keys <- join_keys(x, y, by, mutating = TRUE, never_na = na_matches == "never")
  rows <- join_rows(keys, type)

  x_names <- names(x)
  y_names <- names(y)
  x_data <- as.list(.subset(x))
  x_data[match(by$x, x_names)] <- keys$x
  y_data <- as.list(.subset(y))
  y_data[match(by$y, y_names)] <- keys$y
  out_x <- lapply(x_data, vec_slice, i = rows$x)
  if (keep) {
    out_names <- c(suffix_clashes(x_names, y_names, suffix[1L]), suffix_clashes(y_names, x_names, suffix[2L]))
  } else {
    # Rows found only in `y` take their key values from `y`, and the keys of `y` are dropped
    y_only <- which(is.na(rows$x))
    for (i in seq_along(by$x)) {
      pos <- match(by$x[i], x_names)
      out_x[[pos]][y_only] <- vec_slice(keys$y[[i]], rows$y[y_only])
    }
    is_x_key <- x_names %in% by$x
    x_out <- x_names
    x_out[!is_x_key] <- suffix_clashes(x_names[!is_x_key], c(by$x, setdiff(y_names, c(by$x, by$y))), suffix[1L])
    is_y_key <- y_names %in% by$y
    out_names <- c(x_out, suffix_clashes(y_names, x_names, suffix[2L])[!is_y_key])
    y_data <- y_data[!is_y_key]
  }
  out <- c(out_x, lapply(y_data, vec_slice, i = rows$y))
  names(out) <- out_names
  reconstruct_attrs(new_data_frame(out, length(rows$x)), x)
}

#' Cast the keys of a join and identify their values
#'
#' @param x,y The `data.frame`s to join.
#' @param by A `list` with `x` and `y`, the names of the key columns, as returned by `join_key_names()`.
#' @param mutating `logical(1)`. Is the join a mutating join? These report incompatible types differently.
#' @param never_na `logical(1)`. Should keys containing `NA` or `NaN` never match?
#' @return A `list` with
#' * `x` and `y`: `list`s of the key columns of `x` and `y`, cast to their common types;
#' * `x_ids` and `y_ids`: an `integer` id for each row of `x` and of `y`, which is the same for rows with the same
#'   keys and `NA` for rows which can never match;
#' * `n`: the number of distinct ids.
#' @noRd
join_keys <- function(x, y, by, mutating = FALSE, never_na = FALSE) {
  n_x <- nrow(x)
  casts <- lapply(seq_along(by$x), function(i) {
    join_key_cast(.subset2(x, by$x[i]), .subset2(y, by$y[i]), by$x[i], mutating = mutating)
  })
  keys <- lapply(casts, `[[`, "key")
  ids <- group_ids(keys, n = n_x + nrow(y))
  if (never_na && length(keys) > 0L) ids[Reduce(`|`, lapply(keys, is.na))] <- NA_integer_
  list(
    x = lapply(casts, `[[`, "x"),
    y = lapply(casts, `[[`, "y"),
    x_ids = ids[seq_len(n_x)],
    y_ids = ids[n_x + seq_len(nrow(y))],
    n = max(ids, 0L, na.rm = TRUE)
  )
}

#' Find the matching rows of a join
#'
#' @param keys The keys of the join, as returned by `join_keys()`.
#' @param type `character(1)`. One of `"inner"`, `"left"`, `"right"` or `"full"`.
#' @return A `list` with `x` and `y`, the row of `x` and of `y` for each row of the result, which are `NA` where the
#' row has no match. The rows follow `x`, with the matches of each row in the order of `y`; rows of `y` without a
#' match come last.
#' @noRd
join_rows <- function(keys, type) {
  x_ids <- keys$x_ids
  y_ids <- keys$y_ids
  n_x <- length(x_ids)
  n_groups <- keys$n

  # Rather than matching row by row, sort `y` by key once: the matches of a key are then a run of `y_sorted`
  y_counts <- tabulate(y_ids, n_groups)
  y_sorted <- order(y_ids)
  y_starts <- cumsum(c(1L, y_counts))[seq_len(n_groups)]
  n_matches <- y_counts[x_ids]
  n_matches[is.na(n_matches)] <- 0L
  n_rows <- if (type %in% c("left", "full")) pmax(n_matches, 1L) else n_matches

  x_rows <- rep.int(seq_len(n_x), n_rows)
  y_rows <- y_sorted[rep.int(y_starts[x_ids], n_rows) + sequence(n_rows) - 1L]
  y_rows[rep.int(n_matches == 0L, n_rows)] <- NA_integer_
  if (type %in% c("right", "full")) {
    matched <- logical(n_groups)
    matched[x_ids] <- TRUE
    unmatched <- which(is.na(y_ids) | !matched[y_ids])
    x_rows <- c(x_rows, rep(NA_integer_, length(unmatched)))
    y_rows <- c(y_rows, unmatched)
  }
  list(x = x_rows, y = y_rows)
}

#' Add a suffix to clashing names
#'
#' A name clashes when it is one of `avoid` or repeats an earlier name. Clashing names are suffixed, and the check is
#' repeated, until no name clashes.
#'
#' @param names `character(n)`. The names to make unique.
#' @param avoid `character(n)`. The names which `names` must not use.
#' @param suffix `character(1)`. The suffix.
#' @noRd
suffix_clashes <- function(names, avoid, suffix) {
  if (!nzchar(suffix)) return(names)
  repeat {
    clash <- names %in% avoid | duplicated(names)
    if (!any(clash)) return(names)
    names[clash] <- paste0(names[clash], suffix)
  }
}

#' Find the key columns of a join
#'
#' @param by The `by` argument of a join: `NULL` for a natural join, otherwise a character vector whose names, where
#' given, are the keys in `x`.
#' @param x_names,y_names `character(n)`. The column names of `x` and `y`.
#' @return A `list` with `x` and `y`, the names of the key columns in `x` and `y`.
#' @noRd
join_key_names <- function(by, x_names, y_names) {
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
  check_join_columns(by_x, x_names)
  check_join_columns(by_y, y_names)
  list(x = unname(by_x), y = by_y)
}

join_message <- function(by) {
  if (length(by) > 1L) {
    message("Joining, by = c(\"", paste0(by, collapse = "\", \""), "\")\n", sep = "")
  } else {
    message("Joining, by = \"", by, "\"\n", sep = "")
  }
}

check_join_columns <- function(vars, names) {
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
  if (is_all_na_logical(x_col) && !is_all_na_logical(y_col)) x_col <- y_col[rep(NA_integer_, length(x_col))]
  if (is_all_na_logical(y_col)) y_col <- x_col[rep(NA_integer_, length(y_col))]

  x_class <- join_key_class(x_col)
  y_class <- join_key_class(y_col)
  if (x_class[["kind"]] != y_class[["kind"]]) {
    x_type <- x_class[["type"]]
    y_type <- y_class[["type"]]
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
    x_class[["kind"]],
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

#' Classify a join key
#'
#' @param x A key column.
#' @return A `character(2)` with `kind`, which keys of the same kind can be cast to a common type, and `type`, the
#' name of the type of the key in errors.
#' @noRd
join_key_class <- function(x) {
  type <- if (is.ordered(x)) {
    "ordered"
  } else if (is.factor(x)) {
    "factor"
  } else if (inherits(x, "Date")) {
    "date"
  } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
    "datetime"
  } else if (is.null(attr(x, "class"))) {
    typeof(x)
  } else {
    class(x)[1L]
  }
  kind <- if (is.character(x) || is.factor(x)) {
    "character"
  } else if (type %in% c("date", "datetime")) {
    "datetime"
  } else if (type %in% c("logical", "integer", "double") && is.null(attr(x, "class"))) {
    "numeric"
  } else {
    paste(class(x), collapse = "/")
  }
  c(kind = kind, type = type)
}
