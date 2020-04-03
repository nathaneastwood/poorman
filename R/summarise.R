#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarising the variables of an existing `data.frame`. Grouped `data.frame`s will
#' result in one row in the output for each group.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result. The value
#' should be an expression that returns a single value, e.g. `min(x)`.
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
summarise.data.frame <- function(.data, ...) {
  fns <- vapply(substitute(...()), deparse, NA_character_)
  res <- lapply(fns, function(x) eval(parse(text = paste("with(.data,", x, ")"))))
  res <- as.data.frame(res)
  fn_names <- names(fns)
  colnames(res) <- if (is.null(fn_names)) fns else fn_names
  res
}

#' @export
summarise.grouped_data <- function(.data, ...) {
  groups <- attr(.data, "groups", exact = TRUE)
  fns <- deparse_dots(...)
  fn_names <- names(fns)
  group_data <- lapply(groups, function(x, .data) extract2(.data, x), .data)
  res <- do.call(rbind, lapply(split(.data, group_data), function(x, groups, fns, fn_names) {
    if (nrow(x) == 0L) return()
    grp_res <- data.frame(
      eval(parse(text = paste0("with(x, list(", paste0(fns, collapse = ", "), "))")))
    )
    grp_res <- cbind(unique(x[, groups]), grp_res)
    colnames(grp_res) <- c(groups, if (is.null(fn_names)) fns else fn_names)
    grp_res
  }, groups, fns, fn_names))
  res <- eval(parse(text = paste0("res[order(", paste0("res[, ", seq_along(groups), "]", collapse = ", "), "), ]")))
  rownames(res) <- NULL
  structure(res, class = c("grouped_data", class(res)), groups = groups)
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @export
summarize.data.frame <- summarise.data.frame
#' @export
summarize.grouped_data <- summarise.grouped_data
