#' Get a glimpse of your data
#'
#' `glimpse()` is like a transposed version of print(): columns run down the page, and data runs across. This makes it
#' possible to see every column in a `data.frame`. It is no more than a wrapper around [utils::str()] only it returns
#' the input (invisibly) meaning it can be used within a data pipeline.
#'
#' @param x An object to glimpse at.
#' @param width `integer(1)`. Width of the output.
#' @param ... Additional parameters to pass to [utils::str()].
#'
#' @return
#' `x`, invisibly.
#'
#' @examples
#' glimpse(mtcars)
#'
#' @export
glimpse <- function(x, width = getOption("width"), ...) {
  UseMethod("glimpse")
}

#' @export
glimpse.default <- function(x, width = getOption("width"), max.level = 3, ...) {
  utils::str(x, width = width, max.level = max.level, ...)
  invisible(x)
}

#' @export
glimpse.data.frame <- function(x, width = getOption("width"), ...) {
  utils::str(x, width = width, ...)
  invisible(x)
}
