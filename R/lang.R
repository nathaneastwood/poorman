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

is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

is_wholenumber <- function(x) {
  x %% 1L == 0L
}

names_are_invalid <- function(x) {
  x == "" | is.na(x)
}

is_named <- function(x) {
  nms <- names(x)
  if (is.null(nms)) return(FALSE)
  if (any(names_are_invalid(nms))) return(FALSE)
  TRUE
}

have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) rep(FALSE, length(x)) else !names_are_invalid(nms)
}

is_empty_list <- function(x) {
  inherits(x, "list") && length(x) == 0L
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

#' Check whether the input is an atomic vector or a data.frame
#' @noRd
is_df_or_vector <- function(x) {
  res <- is.data.frame(x) || is.atomic(x)
  if (!isTRUE(res)) stop("You must pass vector(s) and/or data.frame(s).")
  TRUE
}
