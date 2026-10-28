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

#' Cast a pair of join keys to their common type
#'
#' Factors with different levels combine to a factor of all levels, a
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
  # A Date is cast to a POSIXct at midnight in the time zone of the other key
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
