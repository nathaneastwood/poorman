context <- new.env()

# Data
context$setup <- function(.data) context$.data <- .data
context$get_data <- function() context$.data
context$get_columns <- function(cols) context$.data[, cols, drop = FALSE]
context$cur_column <- NULL
context$get_nrow <- function() nrow(context$.data)
context$get_colnames <- function() colnames(context$.data)
context$is_grouped <- function() has_groups(context$.data)
context$as_env <- function() {
  if (any(is_nested(context$.data))) {
    lapply(as.list(context$.data), function(x) if (is.data.frame(x[[1]])) x[[1]] else x)
  } else {
    context$.data
  }
}
context$clean <- function() {
  rm(list = c(".data"), envir = context)
  if (!is.null(context$cur_column)) rm(list = c("cur_column"), envir = context)
}

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
#' gf %>% summarise(data = list(cur_data_all()))
#'
#' gf %>% mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
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
  check_context("`n()`", context$.data)
  context$get_nrow()
}

#' @description
#' * `cur_data()` gives the current data for the current group (excluding grouping variables).
#' @rdname context
#' @export
cur_data <- function() {
  check_context("`cur_data()`", context$.data)
  data <- context$get_data()
  data[, !(colnames(data) %in% get_groups(data)), drop = FALSE]
}

#' @description
#' * `cur_data_all()` gives the current data for the current group (including grouping variables).
#' @rdname context
#' @export
cur_data_all <- function() {
  check_context("`cur_data_all()`", context$.data)
  ungroup(context$get_data())
}

#' @description
#' * `cur_group()` gives the group keys, a single row `data.frame` containing a column for each grouping variable and
#'   its value.
#' @rdname context
#' @export
cur_group <- function() {
  check_context("`cur_group()`", context$.data)
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  rownames(res) <- NULL
  res
}

#' @description
#' * `cur_group_id()` gives a unique numeric identifier for the current group.
#' @rdname context
#' @export
cur_group_id <- function() {
  check_context("`cur_group_id()`", context$.data)
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  details <- get_group_details(data)
  details[, ".group_id"] <- seq_len(nrow(details))
  res <- suppressMessages(semi_join(details, res))
  res[, ".group_id"]
}

#' @description
#' * `cur_group_rows()` gives the rows the groups appear in the data.
#' @rdname context
#' @export
cur_group_rows <- function() {
  check_context("`cur_group_rows()`", context$.data)
  data <- context$get_data()
  res <- data[1L, get_groups(data), drop = FALSE]
  res <- suppressMessages(semi_join(get_group_details(data), res))
  unlist(res[, ".rows"])
}

cur_column <- function() {
  check_context("`cur_column()`", context$cur_column, "`across`")
  context$cur_column
}

## -- Helpers ----------------------------------------------------------------------------------------------------------

check_context <- function(fn, context, name = NULL) {
  if (is.null(context)) {
    stop(fn, " must only be used inside ", if (is.null(name)) "poorman verbs" else name)
  }
}
