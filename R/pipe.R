#' Forward-pipe operator
#'
#' Pipe an object forward into a function or call expression.
#'
#' @param lhs The result you are piping.
#' @param rhs Where you are piping the result to.
#'
#' @examples
#' # Basic use:
#' iris %>% head
#'
#' # Use with lhs as first argument
#' iris %>% head(10)
#'
#' # Using the dot place-holder
#' "Ceci n'est pas une pipe" %>% gsub("une", "un", .)
#'
#' # When dot is nested, lhs is still placed first:
#' sample(1:10) %>% paste0(LETTERS[.])
#'
#' # This can be avoided:
#' rnorm(100) %>% {c(min(.), mean(.), max(.))} %>% floor
#'
#' # Lambda expressions:
#' iris %>%
#'   {
#'     size <- sample(1:10, size = 1)
#'     rbind(head(., size), tail(., size))
#'   }
#'
#' # renaming in lambdas:
#' iris %>%
#'   {
#'     my_data <- .
#'     size <- sample(1:10, size = 1)
#'     rbind(head(my_data, size), tail(my_data, size))
#'   }
#'
#' @name pipe
#'
#' @author
#' Nathan Eastwood and Antoine Fabri \email{antoine.fabri@@gmail.com}.
#'
#' @export
`%>%` <- function(lhs, rhs) {
  rhs_call <- insert_dot(substitute(rhs))
  eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
}

#' @author Antoine Fabri
#' @noRd
insert_dot <- function(expr) {
  if (is.symbol(expr) || expr[[1]] == quote(`(`)) {
    # if a symbol or an expression inside parentheses, make it a call with dot
    # arg
    expr <- as.call(c(expr, quote(`.`)))
  } else if (length(expr) == 1) {
    # if a call without an arg, give it a dot arg
    expr <- as.call(c(expr[[1]], quote(`.`)))
  } else if (
    expr[[1]] != quote(`{`) &&
    !any(vapply(expr[-1], identical, quote(`.`), FUN.VALUE = logical(1))) &&
    !any(vapply(expr[-1], identical, quote(`!!!.`), FUN.VALUE = logical(1)))
  ) {
    # if a call with args but no dot in arg, insert one first
    expr <- as.call(c(expr[[1]], quote(`.`), as.list(expr[-1])))
  }
  expr
}
