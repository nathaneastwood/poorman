#' Forward-pipe operator
#'
#' Pipe an object forward into a function or call expression.
#'
#' Unlike the `magrittr` pipe, you must supply an actual function instead of just a function name. For example
#' `mtcars %>% head` will not work, but `mtcars %>% head()` will.
#'
#' @param lhs The result you are piping.
#' @param rhs Where you are piping the result to.
#'
#' @examples
#' mtcars %>% head()
#' mtcars %>% select(mpg)
#'
#' @name pipe
#'
#' @export
`%>%` <- function(lhs, rhs) {
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  eval(as.call(c(rhs[[1L]], lhs, as.list(rhs[-1L]))), envir = parent.frame())
}
