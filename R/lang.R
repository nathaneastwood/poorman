as_function <- function(x, env = parent.frame()) {
  if (is.function(x)) return(x)
  if (is_formula(x)) {
    if (length(x) > 2) stop("Can't convert a two-sided formula to a function")
    env <- attr(x, ".Environment", exact = TRUE)
    rhs <- as.list(x)[[2]]
    return(as.function(list(... = substitute(), .x = quote(..1), .y = quote(..2), . = quote(..1), rhs), envir = env))
  }
  if (is_string(x)) return(get(x, envir = env, mode = "function"))
  stop("Can't convert an object of class ", class(x), " to a function.")
}

is_formula <- function(x) {
  inherits(x, "formula")
}

#' Check whether two objects are the same lambda, whatever their environments
#'
#' A lambda written in an expression is a new formula each time the expression is evaluated, so two formulas made by
#' the same code differ in their environment alone. `as_function()` gives such formulas the same function but for its
#' environment.
#'
#' @param x,y Objects to compare.
#' @return `logical(1)`. `TRUE` if `x` and `y` are both one-sided formulas with the same right-hand side.
#' @noRd
is_same_lambda <- function(x, y) {
  is_formula(x) && length(x) == 2L && is_formula(y) && identical(x[[2L]], y[[2L]])
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

is_wholenumber <- function(x) {
  x %% 1L == 0L
}

names_are_invalid <- function(x) {
  x == "" | is.na(x)
}

have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) rep(FALSE, length(x)) else !names_are_invalid(nms)
}

#' Turn an am atomic vector into a list of symbols
#'
#' @return
#' A `list` of symbols.
#'
#' @example
#' as_symbols(c("am", "cyl"))
#'
#' @noRd
as_symbols <- function(x) {
  lapply(x, as.symbol)
}

#' Build one call which evaluates several expressions together
#'
#' `filter()` and `slice()` give each group all of their expressions before the next group, so they evaluate them as
#' one expression, a call to `list()`, through `eval_groups()`.
#'
#' @param exprs A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @return A call to `base::list()` with `exprs` as its unnamed arguments.
#' @noRd
list_call <- function(exprs) {
  as.call(c(list(base::list), unname(exprs)))
}
