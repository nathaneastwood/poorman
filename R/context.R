context <- new.env()

# Data
context$set_data <- function(.data) context$.data <- .data
context$get_data <- function() context$.data
context$get_nrow <- function() nrow(context$.data)
context$get_colnames <- function() colnames(context$.data)
context$clean <- function() rm(list = c(".data"), envir = context)

#' Context dependent expressions
#'
#' @description
#' These functions return information about the "current" group or "current" variable, so only work inside specific
#' contexts like [summarise()] and [mutate()].
#'
#' @section `data.table`:
#' If you're familiar with `data.table`:
#'
#' * `cur_data()` <-> `.SD`
#' * `cur_group_id()` <-> `.GRP`
#' * `cur_group()` <-> `.BY`
#' * `cur_group_rows()` <-> `.I`
#'
#' @examples
#' df <- data.frame(
#'   g = sample(rep(letters[1:3], 1:3)),
#'   x = runif(6),
#'   y = runif(6),
#'   stringsAsFactors = FALSE
#' )
#' gf <- df %>% group_by(g)
#'
#' gf %>% summarise(n = n())
#'
#' gf %>% mutate(id = cur_group_id())
#' gf %>% summarise(row = cur_group_rows())
#' gf %>% summarise(data = list(cur_group()))
#' gf %>% summarise(data = list(cur_data()))
#'
#' @seealso
#' See [group_data()] for equivalent functions that return values for all groups.
#'
#' @name context
NULL

#' @description
#' * `n()` gives the number of observations in the current group.
#' @rdname context
#' @export
n <- function() {
  check_group_context("`n()`")
  context$get_nrow()
}

#' @description
#' * `cur_data()` gives the current data for the current group (exclusing grouping variables).
#' @rdname context
cur_data <- function() {
  check_group_context("`cur_data()`")
  data <- context$get_data()
  data[, !(colnames(data) %in% get_groups(data)), drop = FALSE]
}

#' @description
#' * `cur_group()` gives the group keys, a single row `data.frame` containing a column for each grouping variable and
#'   its value.
#' @rdname context
cur_group <- function() {
  check_group_context("`cur_group()`")
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  rownames(res) <- NULL
  res
}

#' @description
#' * `cur_group_id()` gives a unique numeric identifier for the current group.
#' @rdname context
cur_group_id <- function() {
  check_group_context("`cur_group_id()`")
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  details <- get_group_details(data)
  details[, ".group_id"] <- seq_len(nrow(details))
  res <- suppressMessages(semi_join(details, res))
  list(res[, ".group_id"])
}

#' @description
#' * `cur_group_rows()` gives the rows the groups appear in the data.
#' @rdname context
cur_group_rows <- function() {
  check_group_context("`cur_group_rows()`")
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  res <- suppressMessages(semi_join(get_group_details(data), res))
  unlist(res[, ".rows"])
}

check_group_context <- function(fn) {
  if (is.null(context$.data)) {
    stop(fn, " must only be used inside poorman verbs")
  }
}
