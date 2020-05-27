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
#' @name slice
NULL

#' @rdname slice
#' @export
slice <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("slice")
}

#' @export
slice.data.frame <- function(.data, ...) {
  if (nrow(.data) == 0L) return(.data)
  pos <- slice_positions(.data, ...)
  .data[pos, , drop = FALSE]
}

#' @export
slice.grouped_data <- function(.data, ...) {
  apply_grouped_function("slice", .data, ...)
}


#' @rdname slice
#' @export
slice_head <- function(.data, ..., n, prop) {
  UseMethod("slice_head")
}

#' @export
slice_head.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  idx <- switch(
    size$type,
    n = function(n) seq2(1, min(size$n, n)),
    prop = function(n) seq2(1, min(size$prop * n, n))
  )
  slice(.data, idx(poorman::n()))
}

#' @export
slice_head.grouped_data <- function(.data, ..., n, prop) {
  apply_grouped_function("slice_head", .data, n = n, prop = prop, ...)
}

#' @rdname slice
#' @export
slice_tail <- function(.data, ..., n, prop) {
  UseMethod("slice_tail")
}

#' @export
slice_tail.data.frame <- function(.data, ..., n, prop) {
  size <- check_slice_size(n, prop)
  idx <- switch(
    size$type,
    n = function(n) seq2(max(n - size$n + 1, 1), n),
    prop = function(n) seq2(max(ceiling(n - size$prop * n) + 1, 1), n)
  )
  slice(.data, idx(poorman::n()))
}

#' @export
slice_tail.grouped_data <- function(.data, ..., n, prop) {
  apply_grouped_function("slice_tail", .data, n = n, prop = prop, ...)
}

#' @rdname slice
#' @export
slice_min <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_min")
}

#' @param order_by The variable to order by.
#' @param with_ties `logical(1)`. Should ties be kept together? The default, `TRUE`, may return more rows than you
#' request. Use `FALSE` to ignore ties, and return the first `n` rows.
#'
#' @rdname slice
#' @export
slice_min.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) stop("argument `order_by` is missing, with no default.")

  size <- check_slice_size(n, prop)
  idx <- if (isTRUE(with_ties)) {
    switch(
      size$type,
      n = function(x, n) vec_head(order(x), smaller_ranks(x, size$n)),
      prop = function(x, n) vec_head(order(x), smaller_ranks(x, size$prop * n))
    )
  } else {
    switch(
      size$type,
      n = function(x, n) vec_head(order(x), size$n),
      prop = function(x, n) vec_head(order(x), size$prop * n)
    )
  }
  order_by <- .data[, deparse_var(order_by)]
  slice(.data, idx(order_by, poorman::n()))
}

#' @export
slice_min.grouped_data <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  eval_env$env <- environment()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  apply_grouped_function("slice_min", .data, order_by = order_by, n = n, prop = prop, with_ties = with_ties, ...)
}

#' @rdname slice
#' @export
slice_max <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  UseMethod("slice_max")
}

#' @export
slice_max.data.frame <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) stop("argument `order_by` is missing, with no default.")

  size <- check_slice_size(n, prop)
  idx <- if (isTRUE(with_ties)) {
    switch(
      size$type,
      n = function(x, n) vec_head(order(x, decreasing = TRUE), smaller_ranks(desc(x), size$n)),
      prop = function(x, n) vec_head(order(x, decreasing = TRUE), smaller_ranks(desc(x), size$prop * n))
    )
  } else {
    switch(
      size$type,
      n = function(x, n) vec_head(order(x, decreasing = TRUE), size$n),
      prop = function(x, n) vec_head(order(x, decreasing = TRUE), size$prop * n)
    )
  }
  order_by <- .data[, deparse_var(order_by)]
  slice(.data, idx(order_by, poorman::n()))
}

#' @export
slice_max.grouped_data <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  eval_env$env <- environment()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  apply_grouped_function("slice_max", .data, order_by = order_by, n = n, prop = prop, with_ties = with_ties, ...)
}

#' @param replace `logical(1)`. Should sampling be performed with (`TRUE`) or without (`FALSE`, the default)
#' replacement.
#' @param weight_by Sampling weights. This must evaluate to a vector of non-negative numbers the same length as the
#' input. Weights are automatically standardised to sum to 1.
#'
#' @rdname slice
#' @export
slice_sample <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  UseMethod("slice_sample")
}

#' @export
slice_sample.data.frame <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  size <- check_slice_size(n, prop)
  idx <- switch(
    size$type,
    n = function(x, n) sample_int(n, size$n, replace = replace, wt = x),
    prop = function(x, n) sample_int(n, size$prop * n, replace = replace, wt = x),
  )
  weight_by <- deparse_var(weight_by)
  if (!is.null(weight_by)) weight_by <- .data[, weight_by]
  slice(.data, idx(weight_by, poorman::n()))
}

#' @export
slice_sample.grouped_data <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  eval_env$env <- environment()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  apply_grouped_function("slice_sample", .data, n = n, prop = prop, weight_by = weight_by, replace = replace, ...)
}

# helpers ----------------------------------------------------------------------

slice_positions <- function(.data, ...) {
  conditions <- dots_to_list(...)
  context$set_data(.data)
  on.exit(context$clean(), add = TRUE)
  if (length(conditions) == 0L) return(seq_len(n()))

  frame <- parent.frame(2L)
  rows <- lapply(
    conditions,
    function(cond, frame) {
      res <- eval(cond, context$.data, frame)
      if (is.logical(res) && all(is.na(res))) {
        res <- integer()
      } else if (is.numeric(res)) {
        res <- as.integer(res)
      } else if (!is.integer(res)) {
        stop("`slice()` expressions should return indices (positive or negative integers).")
      }
    },
    frame = frame
  )
  rows <- do.call(c, rows)
  if (length(rows) == 0L) {
    # do nothing
  } else if (all(rows >= 0, na.rm = TRUE)) {
    rows <- rows[!is.na(rows) & rows <= n() & rows > 0]
  } else if (all(rows <= 0, na.rm = TRUE)) {
    rows <- setdiff(seq_len(n()), -rows)
  } else {
    stop("`slice()` expressions should return either all positive or all negative.")
  }
  rows
}

check_slice_size <- function(n, prop) {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    if (!is.numeric(n) || length(n) != 1) {
      stop("`n` must be a single number.")
    }
    if (is.na(n) || n < 0) {
      stop("`n` must be a non-missing positive number.")
    }

    list(type = "n", n = n)
  } else if (!missing(prop) && missing(n)) {
    if (!is.numeric(prop) || length(prop) != 1) {
      stop("`prop` must be a single number.")
    }
    if (is.na(prop) || prop < 0) {
      stop("`prop` must be a non-missing positive number.")
    }
    list(type = "prop", prop = prop)
  } else {
    stop("Must supply exactly one of `n` and `prop` arguments.")
  }
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  if (isTRUE(replace)) {
    sample.int(n, size, prob = wt, replace = TRUE)
  } else {
    sample.int(n, min(size, n), prob = wt)
  }
}

smaller_ranks <- function(x, y) {
  sum(min_rank(x) <= y, na.rm = TRUE)
}
