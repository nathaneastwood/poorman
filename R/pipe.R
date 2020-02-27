#' Pipe
#'
#' @param lhs The result you are piping.
#' @param rhs Where you are piping the result to.
#'
#' @examples
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
