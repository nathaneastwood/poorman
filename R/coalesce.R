#' Find first non-missing element
#'
#' Given a set of vectors, `coalesce()` finds the first non-missing value at each position. This is inspired by the SQL
#' `COALESCE` function which does the same thing for `NULL`s.
#'
#' @details
#' Currently, `coalesce()` type checking does not take place.
#'
#' @param ... Vectors. Inputs should be recyclable (either be length `1L` or `n`) and coercible to a common type.
#'
#' @examples
#' # Use a single value to replace all missing vectors
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0L)
#'
#' # Or match together a complete vector from missing pieces
#' y <- c(1, 2, NA, NA, 5)
#' z <- c(NA, NA, 3, 4, 5)
#' coalesce(y, z)
#'
#' @export
coalesce <- function(...) {
  if (missing(..1)) stop("At least one argument must be supplied.")

  vectors <- list(...)
  vectors_lens <- unique(lengths(vectors))
  if (length(vectors_lens) > 2L || (length(vectors_lens) == 2L & !1 %in% vectors_lens)) {
    stop("Vectors must all be of length 1 and/or n")
  }
  max_len <- max(vectors_lens)

  len_one <- lengths(vectors) == 1L
  vectors[len_one] <- lapply(vectors[len_one], function(x) rep(x, max_len))

  x <- vectors[[1]]
  vectors <- vectors[-1]

  for (i in seq_along(vectors)) {
    x_miss <- is.na(x)
    x[x_miss] <- vectors[[i]][x_miss]
  }
  x
}
