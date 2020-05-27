context <- new.env()
context$set_data <- function(.data) context$.data <- .data
context$get_colnames <- function() colnames(context$.data)
context$get_nrow <- function() nrow(context$.data)
context$clean <- function() rm(list = c(".data"), envir = context)

eval_env <- new.env()

#' Peek at variables in the selection context
#'
#' Return the vector of column names of the data currently available for selection.
#'
#' @return
#' A vector of column names.
#'
#' @export
peek_vars <- function() {
  context$get_colnames()
}

#' The number of observations in the current group
#'
#' This function can be used within the context of [summarise()], [mutate()] and [filter()].
#'
#' @return
#' An `integer`.
#'
#' @examples
#' mt_gears <- mtcars %>% group_by(gear)
#' mt_gears %>% mutate(n = n())
#' mt_gears %>% filter(n() < 10)
#' mt_gears %>% summarise(n = n())
#'
#' @export
n <- function() {
  rows <- context$get_nrow()
  if (is.null(rows)) stop("`n()` must be used inside one of `summarise()`, `mutate()`, `filter()` or `slice()`")
  rows
}
