#' Summarise
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result. The value
#' should be an expression that returns a single value, e.g. `min(x)`.
#'
#' @examples
#' summarise(mtcars, mean(mpg))
#' summarise(mtcars, meanMpg = mean(mpg), sumMpg = sum(mpg))
#' \dontrun{
#' mtcars %>% summarise(mean(mpg))
#' }
#'
#' @export
summarise <- function(.data, ...) {
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
  res <- do.call(rbind, lapply(split(.data, group_data), function(x, groups, fns) {
    to_get <- paste(paste0(groups, "[[1]]", collapse = ", "), paste(fns, collapse = ", "), sep = ", ")
    eval(parse(text = paste("with(x, list(", to_get, "))")))
  }, groups, fns))
  colnames(res) <- c(groups, if (is.null(fn_names)) fns else fn_names)
  rownames(res) <- NULL
  as.data.frame(res)
}
