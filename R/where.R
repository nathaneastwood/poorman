#' Select variables with a function
#'
#' This selection helper selects the variables for which a function returns `TRUE`.
#'
#' @param fn A function that returns `TRUE` or `FALSE`.
#'
#' @examples
#' iris %>% select(where(is.numeric))
#' iris %>% select(where(function(x) is.numeric(x)))
#' iris %>% select(where(function(x) is.numeric(x) && mean(x) > 3.5))
#'
#' @return A vector of `integer` column positions which are the result of the `fn` evaluation.
#'
#' @seealso [select_helpers]
#'
#' @export
where <- function(fn) {
  if (!is_function(fn)) {
    stop(deparse_var(fn), " is not a valid predicate function.")
  }
  preds <- unlist(lapply(
    context$.data,
    function(x, fn) {
      eval(call("fn", x))
    },
    fn
  ))
  if (!is.logical(preds)) stop("`where()` must be used with functions that return `TRUE` or `FALSE`.")
  data_cols <- context$get_colnames()
  cols <- data_cols[preds]
  which(data_cols %in% cols)
}
