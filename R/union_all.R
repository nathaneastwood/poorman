#' Union All
#'
#' Union all elements of R objects together.
#'
#' @param x,y objects to union all elements of (ignoring order)
#' @param ... other arguments passed on to methods
#'
#' @examples
#' first <- mtcars[1:20, ]
#' second <- mtcars[10:32, ]
#' union_all(first, second)
#'
#' # union_all does not remove duplicates
#' a <- data.frame(column = c(1:10, 10))
#' b <- data.frame(column = c(1:5, 5))
#' union_all(a, b)
#'
#' @export
union_all <- function(x, y, ...) {
  UseMethod("union_all")
}

#' @export
union_all.default <- function(x, y, ...) {
  c(x, y)
}

#' @export
union_all.data.frame <- function(x, y, ...) {
  out <- bind_rows(x, y)
  reconstruct_attrs(out, x)
}
