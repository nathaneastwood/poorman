#' Subset rows by position
#'
#' Subset rows by their original position in the `data.frame`. Grouped `data.frame`s use the position within each group.
#'
#' @param .data A `data.frame`.
#' @param ... For `slice()`: integer row values.
#'
#' Provide either positive values to keep, or negative values to drop. The values provided must be either all positive
#' or negative. Indices beyond the number of rows in the input are silently ignored.
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the proportion of rows to select. If neither are
#' supplied, `n = 1` will be used.
#'
#' If `n` is greater than the number of rows in the group (or `prop > 1`), the result will be silently truncated to the
#' group size. If the `prop`ortion of a group size is not an integer, it is rounded down.
#'
#' A negative value of `n` or `prop` gives the number or proportion of rows to drop from each group, e.g. `n = -2`
#' keeps all rows but the last two in `slice_head()`. Any fractional part of a negative `n` is ignored, and the number
#' of rows a negative `prop` drops is rounded down. `slice_sample()` does not accept a negative `n` or `prop`.
#'
#' @return
#' An object of the same type as `.data`. The output has the following properties:
#'
#' * Each row may appear 0, 1, or many times in the output.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#'
#' @examples
#' slice(mtcars, c(1, 2, 3))
#' mtcars %>% slice(1:3)
#'
#' # Similar to head(mtcars, 1)
#' mtcars %>% slice(1L)
#'
#' # Similar to tail(mtcars, 1):
#' mtcars %>% slice(n())
#' mtcars %>% slice(5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -(1:4))
#'
#' # First and last rows based on existing order
#' mtcars %>% slice_head(n = 5)
#' mtcars %>% slice_tail(n = 5)
#'
#' # Grouped operations:
#' mtcars %>% group_by(am, cyl, gear) %>% slice_head(n = 2)
#'
#' @importFrom utils head
#'
#' @name slice
NULL

#' @rdname slice
#' @export
slice <- function(.data, ...) {
  UseMethod("slice")
}

#' @export
slice.data.frame <- function(.data, ...) {
  dots <- dotdotdot(...)
  if (length(dots) == 0L) return(.data)
  # As in `filter()`, the indices are evaluated as one call, so each group is given all of them before the next group
  slice_rows(.data, slice_positions, list(indices = list_call(dots)), parent.frame())
}

#' @rdname slice
#' @export
slice_head <- function(.data, ..., n, prop) {
  UseMethod("slice_head")
}

#' @export
slice_head.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  slice_rows(.data, function(value, n_rows) seq_len(slice_count(size, n_rows)))
}

#' @rdname slice
#' @export
slice_tail <- function(.data, ..., n, prop) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  slice_rows(.data, function(value, n_rows) seq2(n_rows - slice_count(size, n_rows) + 1L, n_rows))
}

#' @param order_by A variable, or an expression of the variables, to order by, e.g. `x * y`. It must have a value for
#' each row of the group.
#' @param with_ties `logical(1)`. Should ties be kept together? The default, `TRUE`, may return more rows than you
#' request. Use `FALSE` to ignore ties, and return the first `n` rows.
#'
#' @rdname slice
#' @export
slice_min <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_min")
}

#' @export
slice_min.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) stop("argument `order_by` is missing, with no default.")
  positions <- slice_order_positions(check_slice_size(n, prop), with_ties, decreasing = FALSE)
  slice_rows(.data, positions, list(order_by = substitute(order_by)), parent.frame())
}

#' @rdname slice
#' @export
slice_max <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) stop("argument `order_by` is missing, with no default.")
  positions <- slice_order_positions(check_slice_size(n, prop), with_ties, decreasing = TRUE)
  slice_rows(.data, positions, list(order_by = substitute(order_by)), parent.frame())
}

#' @param replace `logical(1)`. Should sampling be performed with (`TRUE`) or without (`FALSE`, the default)
#' replacement.
#' @param weight_by Sampling weights. This must evaluate to a vector of non-negative numbers the same length as the
#' input, or the group on grouped data. Weights are automatically standardised to sum to 1.
#'
#' @rdname slice
#' @export
slice_sample <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  UseMethod("slice_sample")
}

#' @export
slice_sample.data.frame <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  size <- check_slice_size(n, prop, allow_negative = FALSE)
  positions <- function(wt, n_rows) {
    if (!is.null(wt)) check_slice_input_size(wt, "weight_by", n_rows)
    sample.int(n_rows, slice_count(size, n_rows, replace), replace = isTRUE(replace), prob = wt)
  }
  weight_by <- substitute(weight_by)
  slice_rows(.data, positions, if (!is.null(weight_by)) list(weight_by = weight_by), parent.frame())
}

# helpers ----------------------------------------------------------------------

#' Select rows by their position within each group
#'
#' The positions of the rows to keep are found within each group, then turned into positions in `.data` so that the
#' rows are selected once. As in dplyr, the rows of grouped data are returned in group order, and groups left empty are
#' dropped unless the groups were made with `.drop = FALSE`.
#'
#' @param .data A `data.frame`.
#' @param positions A `function(value, n_rows)` which returns the positions of the rows to keep within a group of
#' `n_rows` rows, given `value`, the result of `dots` for the group, or `NULL` if there are no `dots`.
#' @param dots A named `list` of one unevaluated expression to evaluate for each group, or `NULL` if `positions` only
#' needs the size of each group.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return `.data` with the selected rows.
#' @noRd
slice_rows <- function(.data, positions, dots = NULL, env = NULL) {
  if (.row_names_info(.data, 2L) == 0L) return(.data)
  if (is.null(dots)) {
    layout <- group_layout(.data)
    details <- layout$details
    rows <- layout$rows
    local <- lapply(lengths(rows), function(n_rows) positions(NULL, n_rows))
  } else {
    # The positions of a group can have any size, so only the results of one group are checked against each other
    transform <- function(value, size, group, details) positions(value, size)
    res <- eval_groups(.data, dots, env, common_size_rule, transform = transform)
    details <- res$details
    rows <- res$rows
    local <- res$values[[1L]]
  }
  counts <- lengths(local)
  starts <- cumsum(c(0L, lengths(rows)))[seq_along(rows)]
  idx <- unlist(rows, use.names = FALSE)[rep.int(starts, counts) + as.integer(unlist(local, use.names = FALSE))]
  out <- subset_rows(.data, idx)
  if (anyDuplicated(idx)) attr(out, "row.names") <- make.unique(as.character(attr(.data, "row.names")[idx]))
  if (!is.null(details)) {
    if (nrow(details) > 0L) {
      details[[".rows"]] <- split_rows(rep.int(seq_along(counts), counts), length(counts))
      details <- drop_empty_groups(details)
    }
    attr(out, "groups") <- details
  }
  out
}

#' Find the rows to keep from the indices given to `slice()`
#'
#' @param value A `list` of the results of the indices for a group.
#' @param n_rows `integer(1)`. The number of rows in the group.
#' @return `integer(n)`. Positive indices are kept, ignoring those beyond `n_rows`; negative indices are dropped.
#' Indices which are all missing select no rows.
#' @noRd
slice_positions <- function(value, n_rows) {
  rows <- lapply(value, function(res) {
    if (is.logical(res) && all(is.na(res))) return(integer())
    if (!is.numeric(res)) {
      stop("`slice()` expressions should return indices (positive or negative integers).", call. = FALSE)
    }
    as.integer(res)
  })
  rows <- unlist(rows, use.names = FALSE)
  if (length(rows) == 0L) return(integer())
  if (all(rows >= 0L, na.rm = TRUE)) return(rows[!is.na(rows) & rows <= n_rows & rows > 0L])
  if (all(rows <= 0L, na.rm = TRUE)) return(setdiff(seq_len(n_rows), -rows))
  stop("`slice()` expressions should return either all positive or all negative.", call. = FALSE)
}

#' Build the function which finds the smallest or largest rows of a group
#'
#' @param size The size of the slice, as returned by `check_slice_size()`.
#' @param with_ties `logical(1)`. Should rows which tie with the last row be kept?
#' @param decreasing `logical(1)`. Find the largest rows, as `slice_max()`, rather than the smallest?
#' @return A `function(x, n_rows)`, for `slice_rows()`, of the values `x` to order the `n_rows` rows of a group by.
#' Missing values are ordered last and are never tied.
#' @noRd
slice_order_positions <- function(size, with_ties, decreasing) {
  function(x, n_rows) {
    check_slice_input_size(x, "order_by", n_rows)
    if (decreasing) x <- desc(x)
    k <- slice_count(size, n_rows)
    if (isTRUE(with_ties)) k <- smaller_ranks(x, k)
    head(order(x), k)
  }
}

#' Find the number of rows that `n` or `prop` selects from a group
#'
#' @param size The size of the slice, as returned by `check_slice_size()`.
#' @param n_rows `integer(1)`. The number of rows in the group.
#' @param replace `logical(1)`. Are rows sampled with replacement, so that more than `n_rows` can be selected?
#' @return `integer(1)`, rounded down and, unless `replace = TRUE`, no more than `n_rows`. A negative size gives the
#' rows which remain once that many are dropped, as many as `n_rows` less the size rounded towards zero, or none.
#' @noRd
slice_count <- function(size, n_rows, replace = FALSE) {
  k <- if (size$type == "n") size$n else size$prop * n_rows
  if (k < 0) return(as.integer(max(n_rows - floor(-k), 0)))
  k <- floor(k)
  as.integer(if (isTRUE(replace)) k else min(k, n_rows))
}

#' Check that an argument of a slice helper has a value for each row of the group
#'
#' @param x The value of the argument for a group.
#' @param arg `character(1)`. The name of the argument, e.g. `"order_by"`.
#' @param n_rows `integer(1)`. The number of rows in the group.
#' @return `NULL`, invisibly, or an error.
#' @noRd
check_slice_input_size <- function(x, arg, n_rows) {
  if (length(x) == n_rows) return(invisible())
  stop(sprintf("`%s` must have size %d, not size %d.", arg, n_rows, length(x)), call. = FALSE)
}

#' Check the size given to a slice helper
#'
#' As in dplyr, giving both `n` and `prop` is checked first, then that the one given is a single number. A negative
#' size drops rows, except in `slice_sample()`.
#'
#' @param n,prop The `n` and `prop` arguments of the slice helper, either of which may be missing.
#' @param allow_negative `logical(1)`. May the size be negative?
#' @return A `list` with `type`, `"n"` or `"prop"`, and the size, named by `type`. `n = 1` if neither is given.
#' @noRd
check_slice_size <- function(n, prop, allow_negative = TRUE) {
  if (missing(prop)) {
    size <- list(type = "n", n = if (missing(n)) 1L else check_slice_number(n, "n"))
  } else {
    if (!missing(n)) stop("Must supply `n` or `prop`, but not both.", call. = FALSE)
    size <- list(type = "prop", prop = check_slice_number(prop, "prop"))
  }
  if (!allow_negative && size[[2L]] < 0) stop(sprintf("`%s` must be positive.", size$type), call. = FALSE)
  size
}

#' Check that the `n` or `prop` of a slice helper is a single number
#'
#' @param x The value of the argument.
#' @param arg `character(1)`. The name of the argument, for the error message.
#' @return `x`, or an error if it is not a single, non-missing number.
#' @noRd
check_slice_number <- function(x, arg) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) return(x)
  stop(sprintf("`%s` must be a single number.", arg), call. = FALSE)
}

smaller_ranks <- function(x, y) {
  sum(min_rank(x) <= y, na.rm = TRUE)
}
