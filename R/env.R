select_env <- new.env()

#' Peek at variables in the selection context
#'
#' Return the vector of column names of the data currently available for selection.
#'
#' @return
#' A vector of column names.
#'
#' @export
peek_vars <- function() {
  get(".col_names", envir = select_env)
}

context <- new.env()

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
  do.call(nrow, list(quote(.data)), envir = context)
}
