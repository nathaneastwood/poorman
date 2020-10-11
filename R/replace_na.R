#' Replace missing values
#'
#' Replace missing values in a `data.frame` or `vector`.
#'
#' @param data A `data.frame` or `vector`.
#' @param replace If `data` is a `data.frame`, a named `list` giving the value to replace `NA` with for each column. If
#' `data` is a `vector`, a single value used for replacement.
#' @param ... Additional arguments passed onto methods; not currently used.
#'
#' @seealso
#' [na_if()] to replace specified values with a `NA`.
#'
#' [coalesce()] to replace missing values within subsequent `vector`(s) of value(s).
#'
#' @return
#' If `data` is a `data.frame`, `replace_na()` returns a `data.frame`. If `data` is a `vector`, `replace_na()` returns a
#' `vector` of class determined by the union of `data` and `replace`.
#'
#' @examples
#' df <- data.frame(x = c(1, 2, NA), y = c("a", NA, "b"), stringsAsFactors = FALSE)
#' df %>% replace_na(list(x = 0, y = "unknown"))
#' df %>% mutate(x = replace_na(x, 0))
#'
#' df$x %>% replace_na(0)
#' df$y %>% replace_na("unknown")
#'
#' @export
replace_na <- function(data, replace, ...) {
  UseMethod("replace_na")
}

#' @export
replace_na.default <- function(data, replace = NA, ...) {
  check_replacement(replace, deparse(substitute(data)))
  data[is.na(data)] <- replace
  data
}

#' @export
replace_na.data.frame <- function(data, replace = list(), ...) {
  stopifnot(is.list(replace))
  replace_vars <- intersect(names(replace), names(data))
  for (var in replace_vars) {
    check_replacement(replace[[var]], var)
    data[[var]][is.na(data[[var]])] <- replace[[var]]
  }
  data
}

check_replacement <- function(x, var) {
  n <- length(x)
  if (n == 1L) return()
  stop("Replacement for `", var, "` is length ", n, ", not length 1")
}
