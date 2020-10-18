#' Count the number of unique values in a set of vectors
#'
#' This is the equivalent of `length(unique(x))` for multiple vectors.
#'
#' @param ... Vectors of values.
#' @param na.rm `logical(1)`. If `TRUE` missing values don't count.
#'
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' length(unique(x))
#' n_distinct(x)
#'
#' @export
n_distinct <- function(..., na.rm = FALSE) {
  res <- do.call(cbind, list(...))
  if (isTRUE(na.rm)) res <- res[!is.na(res), , drop = FALSE]
  nrow(unique(res))
}
