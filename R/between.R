#' Do values in a numeric vector fall in specified range?
#'
#' This is a shortcut for `x >= left & x <= right`.
#'
#' @param x A `numeric` vector of values.
#' @param left,right Boundary values.
#'
#' @examples
#' between(1:12, 7, 9)
#'
#' x <- rnorm(1e2)
#' x[between(x, -1, 1)]
#'
#' @return A `logical` vector the same length as `x`.
#'
#' @export
between <- function(x, left, right) {
  if (!is.null(attr(x, "class")) && !inherits(x, c("Date", "POSIXct"))) {
    warning("`between()` called on numeric vector with S3 class")
  }
  if (!is.double(x)) x <- as.numeric(x)
  x >= as.numeric(left) & x <= as.numeric(right)
}
