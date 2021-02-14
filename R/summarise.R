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
  UseMethod("summarise")
}

#' @export
summarise.data.frame <- function(.data, ...) {
  fns <- dotdotdot(...)
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  groups_exist <- context$is_grouped()
  if (groups_exist) {
    group <- unique(context$get_columns(group_vars(context$.data)))
  }
  if (is_empty_list(fns)) {
    if (groups_exist) return(group) else return(data.frame())
  }
  res <- vector(mode = "list", length = length(fns))
  eval_env <- c(as.list(context$.data), vector(mode = "list", length = length(fns)))
  new_pos <- seq(length(context$.data) + 1L, length(eval_env), 1L)
  for (i in seq_along(fns)) {
    eval_env[[new_pos[i]]] <- do.call(with, list(eval_env, fns[[i]]))
    nms <- if (!is_named(eval_env[[new_pos[i]]])) {
      if (!is.null(names(fns)[[i]])) names(fns)[[i]] else deparse(fns[[i]])
    } else {
      NULL
    }
    if (!is.null(nms)) names(eval_env)[[new_pos[i]]] <- nms
    res[[i]] <- build_data_frame(eval_env[[new_pos[i]]], nms = nms)
  }
  res <- do.call(cbind, res)
  if (groups_exist) res <- cbind(group, res, row.names = NULL)
  res
}

#' @export
summarise.grouped_data <- function(.data, ...) {
  groups <- group_vars(.data)
  res <- apply_grouped_function("summarise", .data, drop = TRUE, ...)
  res <- res[do.call(order, lapply(groups, function(x) res[, x])), , drop = FALSE]
  res <- groups_set(res, groups, group_by_drop_default(res))
  rownames(res) <- NULL
  res
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @export
summarize.data.frame <- summarise.data.frame
#' @export
summarize.grouped_data <- summarise.grouped_data
