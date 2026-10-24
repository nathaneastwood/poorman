#' Capture unevaluated dots
#'
#' Gather the unevaluated dots into a list, storing them as is. An argument written `name := value` is stored as
#' `value`, named `name`.
#'
#' @param ... Arguments to be stored in the list.
#' @param .impute_names `logical(1)`. Whether to fill any missing names of the list with the automatic names of
#' `auto_label()`.
#' @param .env `environment`. The environment from which `gluestick()` templates on the left of `:=` are filled in,
#' i.e. the environment of the user calling the verb.
#'
#' @noRd
dotdotdot <- function(..., .impute_names = FALSE, .env = parent.frame(2L)) {
  dots <- name_colon_equals(eval(substitute(alist(...))), .env)
  if (isTRUE(.impute_names)) {
    names_dots <- names(dots)
    unnamed <- if (is.null(names_dots)) rep(TRUE, length(dots)) else nchar(names_dots) == 0L
    names(dots)[unnamed] <- vapply(dots[unnamed], auto_label, NA_character_, env = .env)
  }
  dots
}

#' Name the arguments written with `:=`
#'
#' `name := value` is another way to write `name = value`, which also allows a name which is not known in advance.
#' The name on the left is either a symbol, taken as it is, or a string, which is a `gluestick()` template filled in
#' from `env`, e.g. `"{var}_mean" := mean(x)`.
#'
#' @param dots A `list` of unevaluated expressions.
#' @param env An `environment` from which to fill in `gluestick()` templates.
#' @return `dots`, with each `:=` call replaced by its right side and named after its left side.
#' @noRd
name_colon_equals <- function(dots, env) {
  is_colon_equals <- vapply(dots, function(x) is.call(x) && identical(x[[1L]], as.name(":=")), NA)
  if (!any(is_colon_equals)) return(dots)
  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- character(length(dots))
  for (i in which(is_colon_equals)) {
    if (nzchar(dot_names[i])) stop("Can't supply both `=` and `:=`", call. = FALSE)
    lhs <- dots[[i]][[2L]]
    dot_names[i] <- if (is.symbol(lhs)) {
      as.character(lhs)
    } else if (is.character(lhs) && length(lhs) == 1L) {
      gluestick(lhs, src = env)
    } else {
      stop("The LHS of `:=` must be a string or a symbol.", call. = FALSE)
    }
    dots[i] <- list(dots[[i]][[3L]])
  }
  names(dots) <- dot_names
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
