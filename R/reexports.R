#' Same as stats::setNames
#' @noRd
set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}

#' Same as utils::head.default
#' @noRd
vec_head <- function (x, n = 6L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
  x[seq_len(n)]
}
