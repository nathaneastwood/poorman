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
  by <- standardise_join_by(by, names(x), names(y))

  n_x <- nrow(x)
  n_y <- nrow(y)
  out <- as.list(.subset(x))
  keys <- vector("list", length(by$x))
  for (i in seq_along(by$x)) {
    cast <- join_key_cast(.subset2(x, by$x[i]), .subset2(y, by$y[i]), by$x[i])
    out[[by$x[i]]] <- cast$x
    keys[[i]] <- cast$key
  }
  ids <- group_ids(keys, n = n_x + n_y)
  x_ids <- ids[seq_len(n_x)]
  y_rows <- split_rows(ids[n_x + seq_len(n_y)], max(ids, 0L))

  y_cols <- if (isTRUE(keep)) names(y) else setdiff(names(y), by$y)
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
