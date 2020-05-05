#' Vectorised if
#'
#' This is a wrapper around `ifelse()` which checks that `true` and `false` are of the same type, making the output more
#' predictable.
#'
#' @param condition A `logical(n)` vector.
#' @param true,false Values to use for `TRUE` and `FALSE` in `condition`. They must either be the same length as
#' `condition` or be length 1. They must also be the same type.
#' @param missing If not `NULL` (the default), this will replace any missing values.
#'
#' @return A vector the same length as `condition` with values for `TRUE` and `FALSE` replaced by those specified in
#' `true` and `false`, respectively.
#'
#' @examples
#' x <- c(-5:5, NA)
#' if_else(x < 0, NA_integer_, x)
#' if_else(x < 0, "negative", "positive", "missing")
#'
#' # Unlike ifelse, if_else preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#' # Attributes are taken from the `true` vector
#' if_else(x %in% c("a", "b", "c"), x, factor(NA))
#'
#' @export
if_else <- function(condition, true, false, missing = NULL) {
  if (!is.logical(condition)) stop("`condition` must be a logical vector.")
  cls_true <- class(true)
  cls_false <- class(false)
  cls_missing <- class(missing)
  if (!identical(cls_true, cls_false)) {
    stop("The class of `true` <", class(true), "> is not the same as the class of `false` <", class(false), ">")
  }
  if (!is.null(missing) && !identical(cls_true, cls_missing)) {
    stop("`missing` must be a ", cls_true, " vector, not a ", cls_missing, " vector.")
  }
  res <- ifelse(condition, true, false)
  if (!is.null(missing)) res[is.na(res)] <- missing
  attributes(res) <- attributes(true)
  res
}
