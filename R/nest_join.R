#' Nest Join
#'
#' `nest_join()` keeps every row and column of `x` and adds one list column. Each element of that column is a
#' `data.frame` holding the rows of `y` whose keys match the row of `x`. A row of `x` without a match in `y` gets a
#' `data.frame` with zero rows.
#'
#' As rows of `x` are never dropped or repeated, the result always has as many rows as `x`. The other joins can be
#' built on top of it. Counting the rows of each nested `data.frame` tells you which rows `semi_join()` and
#' `anti_join()` would keep, and expanding the nested rows back out gives the result of `inner_join()` or, if the empty
#' matches become missing values, `left_join()`.
#'
#' @param x,y The `data.frame`s to join.
#' @param by A character vector naming the key columns. With `NULL`, the default, the keys are every column name that
#' `x` and `y` share, and a message reports them; pass `by` yourself to silence the message.
#'
#' Name the elements of `by` when the keys have different names in the two tables: `by = c("a" = "b")` pairs column
#' `a` of `x` with column `b` of `y`.
#'
#' `by = character()` uses no keys, so every row of `x` is paired with the whole of `y`.
#' @param copy Ignored. Included for compatibility with `dplyr`, where it allows `y` to come from a different data
#' source.
#' @param keep `logical(1)`. Should the key columns of `y` also appear in the nested `data.frame`s?
#' @param name `character(1)`. The name of the new list column. With `NULL`, the default, the column is named after
#' the expression passed as `y`.
#' @param ... Ignored. Included for compatibility with `dplyr`.
#'
#' @return
#' A `data.frame` with the rows and columns of `x` and a list column, `name`, of `data.frame`s. The join keys of `x`
#' are coerced to the common type of the keys in `x` and `y`. Groups are taken from `x`.
#'
#' @examples
#' customers <- data.frame(id = 1:3, customer = c("Ana", "Ben", "Chloe"))
#' orders <- data.frame(id = c(1L, 1L, 3L, 4L), item = c("tea", "jam", "bread", "milk"))
#'
#' # Ben (id 2) has no orders, so gets a zero row data.frame
#' res <- nest_join(customers, orders, by = "id")
#' res$orders
#'
#' # Keep the key column in the nested data and choose the column name
#' nest_join(customers, orders, by = "id", keep = TRUE, name = "purchases")$purchases
#'
#' # Counting the orders of each customer gives semi_join() and anti_join()
#' n_orders <- vapply(res$orders, nrow, integer(1L))
#' res[n_orders > 0L, c("id", "customer")]  # semi_join(customers, orders, by = "id")
#' res[n_orders == 0L, c("id", "customer")] # anti_join(customers, orders, by = "id")
#'
#' # Expanding the orders back out gives inner_join()
#' expand_matches <- function(x, nested) {
#'   n <- vapply(nested, nrow, integer(1L))
#'   cbind(x[rep(seq_len(nrow(x)), n), , drop = FALSE], do.call(rbind, nested), row.names = NULL)
#' }
#' expand_matches(res[c("id", "customer")], res$orders) # inner_join(customers, orders, by = "id")
#'
#' # and left_join() once each empty match becomes a row of missing values
#' filled <- lapply(
#'   res$orders,
#'   function(o) if (nrow(o) == 0L) o[NA_integer_, , drop = FALSE] else o
#' )
#' expand_matches(res[c("id", "customer")], filled) # left_join(customers, orders, by = "id")
#'
#' @export
nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  UseMethod("nest_join")
}

#' @export
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  nest_col <- if (is.null(name)) expr_label(substitute(y)) else name
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
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
  check_join_vars(by_x, names(x))
  check_join_vars(by_y, names(y))

  n_x <- nrow(x)
  n_y <- nrow(y)
  out <- as.list(.subset(x))
  keys <- vector("list", length(by_x))
  for (i in seq_along(by_x)) {
    cast <- join_key_cast(.subset2(x, by_x[i]), .subset2(y, by_y[i]), by_x[i])
    out[[by_x[i]]] <- cast$x
    keys[[i]] <- cast$key
  }
  ids <- group_ids(keys, n = n_x + n_y)
  x_ids <- ids[seq_len(n_x)]
  y_rows <- split_rows(ids[n_x + seq_len(n_y)], max(ids, 0L))

  y_cols <- if (isTRUE(keep)) names(y) else setdiff(names(y), by_y)
  y_data <- structure(as.list(.subset(y, y_cols)), class = "data.frame", row.names = .set_row_names(n_y))
  # Slice `y` once per matched key rather than once per row of `x`; keys missing from `y` give a 0-row data.frame
  used <- unique(x_ids)
  nested <- vector("list", length(y_rows))
  nested[used] <- lapply(y_rows[used], function(rows) {
    res <- y_data[rows, , drop = FALSE]
    rownames(res) <- NULL
    res
  })
  out[[nest_col]] <- nested[x_ids]

  out <- structure(out, class = "data.frame", row.names = .set_row_names(n_x))
  reconstruct_attrs(out, x)
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
#' @return A `list` with `x`, `x_col` cast to the common type, and `key`, the values of `x_col` and then `y_col` in a
#' form that `group_ids()` can compare.
#' @noRd
join_key_cast <- function(x_col, y_col, name) {
  # An all `NA` logical vector has no type of its own and takes the type of the other key
  if (is_unspecified(x_col) && !is_unspecified(y_col)) x_col <- y_col[rep(NA_integer_, length(x_col))]
  if (is_unspecified(y_col)) y_col <- x_col[rep(NA_integer_, length(y_col))]

  x_kind <- join_key_kind(x_col)
  y_kind <- join_key_kind(y_col)
  if (x_kind != y_kind) {
    stop("Can't combine `", name, "` <", join_key_type(x_col), "> and `", name, "` <", join_key_type(y_col), ">.")
  }
  switch(
    x_kind,
    character = join_key_cast_character(x_col, y_col),
    numeric = {
      type <- typeof(c(x_col[0L], y_col[0L]))
      if (typeof(x_col) != type) storage.mode(x_col) <- type
      list(x = x_col, key = c(unclass(x_col), unclass(y_col)))
    },
    datetime = join_key_cast_datetime(x_col, y_col),
    list(x = x_col, key = c(x_col, y_col))
  )
}

join_key_cast_character <- function(x_col, y_col) {
  if (is.factor(x_col) && is.factor(y_col)) {
    lvls <- union(levels(x_col), levels(y_col))
    if (!identical(lvls, levels(x_col))) {
      x_col <- factor(as.character(x_col), levels = lvls, ordered = is.ordered(x_col))
    }
  } else {
    x_col <- as.character(x_col)
  }
  list(x = x_col, key = c(as.character(x_col), as.character(y_col)))
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
  list(x = x_col, key = c(unclass(x_col), unclass(y_col)))
}

join_key_tz <- function(x) {
  tz <- attr(x, "tzone", exact = TRUE)
  if (is.null(tz)) "" else tz[1L]
}

is_unspecified <- function(x) {
  is.logical(x) && length(x) > 0L && is.null(attributes(x)) && all(is.na(x))
}

join_key_kind <- function(x) {
  if (is.character(x) || is.factor(x)) return("character")
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("datetime")
  if (is.null(attr(x, "class")) && (is.logical(x) || is.integer(x) || is.double(x))) return("numeric")
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
