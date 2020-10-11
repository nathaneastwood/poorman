#' Convert values to NA
#'
#' This is a translation of the SQL command `NULLIF`. It is useful if you want to convert an annoying value to `NA`.
#'
#' @param x The vector to modify.
#' @param y The value to replace with `NA`.
#'
#' @seealso
#' [coalesce()] to replace missing values within subsequent `vector`(s) of value(s). [replace_na()] to replace `NA` with
#' a value.
#'
#' [replace_na()] to replace `NA` with a value.
#'
#' [recode()] to more generally replace values.
#'
#' @return
#' A modified version of `x` that replaces any values that are equal to `y` with `NA`.
#'
#' @examples
#' na_if(1:5, 5:1)
#'
#' x <- c(1, -1, 0, 10)
#' 100 / x
#' 100 / na_if(x, 0)
#'
#' y <- c("abc", "def", "", "ghi")
#' na_if(y, "")
#'
#' # na_if() is particularly useful inside mutate(),
#' # and is meant for use with vectors rather than entire data.frames
#' mtcars %>%
#'   mutate(cyl = na_if(cyl, 6))
#'
#' @export
na_if <- function(x, y) {
  y_len <- length(y)
  x_len <- length(x)
  if (!(y_len %in% c(1L, x_len))) stop("`y` must be length ", x_len, " (same as `x`) or 1, not ", y_len)
  x[x == y] <- NA
  x
}
