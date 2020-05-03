#' Compute lagged or leading values
#'
#' Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector. Useful for comparing values behind of or ahead
#' of the current values.
#'
#' @param x A `vector` of values
#' @param n A positive `integer(1)`, giving the number of positions to lead or lag by.
#' @param default The value used for non-existent rows (default: `NA`).
#'
#' @examples
#' lag(1:5)
#' lead(1:5)
#'
#' x <- 1:5
#' data.frame(behind = lag(x), x, ahead = lead(x))
#'
#' # If you want to look more rows behind or ahead, use `n`
#' lag(1:5, n = 1)
#' lag(1:5, n = 2)
#'
#' lead(1:5, n = 1)
#' lead(1:5, n = 2)
#'
#' # If you want to define a value for non-existing rows, use `default`
#' lag(1:5)
#' lag(1:5, default = 0)
#'
#' lead(1:5)
#' lead(1:5, default = 6)
#'
#' @export
lag <- function (x, n = 1L, default = NA) {
  if (inherits(x, "ts")) stop("`x` must be a vector, not a `ts` object, do you want `stats::lag()`?")
  if (length(n) != 1L || !is.numeric(n) || n < 0L) stop("`n` must be a nonnegative integer scalar")
  if (n == 0L) return(x)
  tryCatch(
    storage.mode(default) <- typeof(x),
    warning = function(w) {
      stop("Cannot convert `default` <", typeof(default), "> to `x` <", typeof(x), ">")
    }
  )
  xlen <- length(x)
  n <- pmin(n, xlen)
  res <- c(rep(default, n), x[seq_len(xlen - n)])
  attributes(res) <- attributes(x)
  res
}

#' @rdname lag
#' @export
lead <- function (x, n = 1L, default = NA) {
  if (length(n) != 1L || !is.numeric(n) || n < 0L) stop("n must be a nonnegative integer scalar")
  if (n == 0L) return(x)
  tryCatch(
    storage.mode(default) <- typeof(x),
    warning = function(w) {
      stop("Cannot convert `default` <", typeof(default), "> to `x` <", typeof(x), ">")
    }
  )
  xlen <- length(x)
  n <- pmin(n, xlen)
  res <- c(x[-seq_len(n)], rep(default, n))
  attributes(res) <- attributes(x)
  res
}
