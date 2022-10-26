#' Cumulative versions of any, all, and mean
#'
#' {poorman} provides `cumall()`, `cumany()`, and `cummean()` to complete R's set of cumulative functions.
#'
#' @section Cumulative logical functions:
#'
#' These are particularly useful in conjunction with `filter()`:
#'
#' * `cumall(x)`: all cases until the first `FALSE`.
#' * `cumall(!x)`: all cases until the first `TRUE`.
#' * `cumany(x)`: all cases after the first `TRUE`.
#' * `cumany(!x)`: all cases after the first `FALSE`.
#'
#' @param x For `cumall()` and `cumany()`, a logical vector; for `cummean()` an integer or numeric vector.
#'
#' @return A vector the same length as `x`.
#'
#' @examples
#' # `cummean()` returns a numeric/integer vector of the same length
#' # as the input vector.
#' x <- c(1, 3, 5, 2, 2)
#' cummean(x)
#' cumsum(x) / seq_along(x)
#'
#' # `cumall()` and `cumany()` return logicals
#' cumall(x < 5)
#' cumany(x == 3)
#'
#' # `cumall()` vs. `cumany()`
#' df <- data.frame(
#'   date = as.Date("2020-01-01") + 0:6,
#'   balance = c(100, 50, 25, -25, -50, 30, 120)
#' )
#' # all rows after first overdraft
#' df %>% filter(cumany(balance < 0))
#' # all rows until first overdraft
#' df %>% filter(cumall(!(balance < 0)))
#'
#' @export
cummean <- function(x) {
  res <- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    res[[i]] <- mean(x[seq(from = 1, to = i, by = 1)])
  }
  res
}

#' @rdname cummean
#' @export
cumany <- function(x) {
  x <- as.logical(x)
  res <- vector(mode = "logical", length = length(x))
  for (i in seq_along(x)) {
    res[[i]] <- any(x[seq(from = 1, to = i, by = 1)] == TRUE)
  }
  res
}

#' @rdname cummean
#' @export
cumall <- function(x) {
  if (any(!is.logical(x))) return(rep(NA, length(x)))
  res <- vector(mode = "logical", length = length(x))
  for (i in seq_along(x)) {
    res[[i]] <- all(x[seq(from = 1, to = i, by = 1)] == TRUE)
  }
  res
}
