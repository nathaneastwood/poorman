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
  conditions <- eval(substitute(alist(...)))
  if (length(conditions) == 0L) return(.data)
  if (nrow(.data) == 0L) return(.data)
  context$.data <- .data
  on.exit(rm(.data, envir = context))
  frame <- parent.frame()
  rows <- lapply(
    conditions,
    function(cond, frame) {
      res <- eval(cond, context$.data, frame)
      res[is.na(res)] <- 0L
      res
    },
    frame = frame
  )
  row_class <- vapply(rows, class, NA_character_)
  if (any(!row_class %in% c("integer", "numeric"))) stop("Conditions must evaluate to indicies (positive or negative)")
  rows <- do.call(c, rows)
  if (!all(rows > 0) && !all(rows < 0) && all(rows != 0)) {
    stop("`slice()` expressions should return either all positive or all negative values")
  }
  if (all(rows > 0L)) rows <- rows[rows <= nrow(.data)]
  context$.data[rows, , drop = FALSE]
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
  idx <- switch(size$type,
    n = function(n) seq2(1, min(size$n, n)),
    prop = function(n) seq2(1, min(size$prop * n, n))
  )
  context$.data <- .data
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
  idx <- switch(size$type,
    n = function(n) seq2(max(n - size$n + 1, 1), n),
    prop = function(n) seq2(max(ceiling(n - size$prop * n) + 1, 1), n)
  )
  context$.data <- .data
  slice(.data, idx(poorman::n()))
}

#' @export
slice_tail.grouped_data <- function(.data, ..., n, prop) {
  apply_grouped_function("slice_tail", .data, n = n, prop = prop, ...)
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
