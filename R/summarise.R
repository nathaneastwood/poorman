#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarising the variables of an existing `data.frame`. Grouped `data.frame`s will
#' result in one row in the output for each group.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result.
#'
#' The value can be:
#' * A vector of length `1`, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#' * A vector of length `n`, e.g. `quantile()`.
#'
#' @examples
#' summarise(mtcars, mean(mpg))
#' summarise(mtcars, meanMpg = mean(mpg), sumMpg = sum(mpg))
#' mtcars %>% summarise(mean(mpg))
#'
#' @name summarise
#' @export
summarise <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("summarise")
}

#' @export
summarise.default <- function(.data, ...) {
  fns <- vapply(substitute(...()), deparse, NA_character_)
  context$set_data(.data)
  on.exit(context$clean(), add = TRUE)
  groups_exist <- has_groups(context$.data)
  if (groups_exist) {
    group <- unique(context$.data[, get_groups(context$.data), drop = FALSE])
  }
  res <- lapply(
    fns,
    function(x) {
      x_res <- do.call(with, list(context$.data, str2lang(x)))
      if (is.list(x_res)) I(x_res) else x_res
    }
  )
  res <- as.data.frame(res)
  fn_names <- names(fns)
  colnames(res) <- if (is.null(fn_names)) fns else fn_names
  if (groups_exist) res <- cbind(group, res, row.names = NULL)
  res
}

#' @export
summarise.grouped_data <- function(.data, ...) {
  groups <- get_groups(.data)
  res <- apply_grouped_function("summarise", .data, drop = TRUE, ...)
  res <- res[do.call(order, lapply(groups, function(x) res[, x])), ]
  rownames(res) <- NULL
  res
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @export
summarize.default <- summarise.default
#' @export
summarize.grouped_data <- summarise.grouped_data
