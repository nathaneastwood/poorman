#' Capture unevaluated dots
#'
#' Gather the unevaluated dots into a list, storing them as is.
#'
#' @param ... Arguments to be stored in the list.
#' @param .impute_names `logical(1)`. Whether to fill any missing names of the list.
#'
#' @noRd
dotdotdot <- function(..., .impute_names = FALSE) {
  dots <- eval(substitute(alist(...)))
  if (isTRUE(.impute_names)) {
    deparse_dots <- lapply(dots, deparse)
    names_dots <- names(dots)
    unnamed <- if (is.null(names_dots)) rep(TRUE, length(dots)) else nchar(names_dots) == 0L
    names(dots)[unnamed] <- deparse_dots[unnamed]
  }
  dots
}

#' Capture unevaluated dots
#'
#' Gather the unevaluated dots into a list, storing them as characters.
#'
#' @param ... Arguments to be stored in the list.
#'
#' @noRd
deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

#' Capture variable
#'
#' Deparse a variable's value
#'
#' @param var An R object.
#' @param frame An `environment` in which `var` should be evaluated.
#'
#' @noRd
deparse_var <- function(var, frame = if (is.null(eval_env$env)) parent.frame() else eval_env$env) {
  sub_var <- eval(substitute(substitute(var)), frame)
  if (is.symbol(sub_var)) var <- as.character(sub_var)
  var
}
