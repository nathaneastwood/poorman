context <- new.env()

# Data
# A verb sets the data of the context with `setup()` and removes it with `clean()` when it exits. The data and column of
# any outer verb are kept and restored by `clean()`, so that a verb can be used inside the expressions of another.
context$setup <- function(.data) {
  context$outer <- list(
    data = get0(".data", envir = context, inherits = FALSE),
    cur_column = context$cur_column,
    outer = context$outer
  )
  context$.data <- .data
}
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
  outer <- context$outer
  context$outer <- outer$outer
  if (is.null(outer$data)) rm(list = ".data", envir = context) else context$.data <- outer$data
  context$cur_column <- outer$cur_column
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
  res <- data[, !(colnames(data) %in% group_vars(data)), drop = FALSE]
  attr(res, "group_id") <- NULL
  res
}

#' @description
#' * `cur_data_all()` gives the current data for the current group (including grouping variables).
#' @rdname context
#' @export
cur_data_all <- function() {
  check_context("`cur_data_all()`", context$.data)
  res <- ungroup(context$get_data())
  attr(res, "group_id") <- NULL
  res
}

#' @description
#' * `cur_group()` gives the group keys, a single row `data.frame` containing a column for each grouping variable and
#'   its value.
#' @rdname context
#' @export
cur_group <- function() {
  check_context("`cur_group()`", context$.data)
  data <- context$get_data()
  id <- attr(data, "group_id", exact = TRUE)
  # An empty group has no rows to take the keys from
  res <- if (!is.null(id) && !is.na(id)) {
    get_group_details(data)[id, group_vars(data), drop = FALSE]
  } else {
    data[1L, group_vars(data), drop = FALSE]
  }
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
  if (!has_groups(data)) return(1L)
  id <- attr(data, "group_id", exact = TRUE)
  if (!is.null(id) && !is.na(id)) return(id)
  res <- data[1L, group_vars(data), drop = FALSE]
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
  if (!has_groups(data)) return(seq_len(nrow(data)))
  id <- attr(data, "group_id", exact = TRUE)
  if (!is.null(id) && !is.na(id)) return(get_group_details(data)[[".rows"]][[id]])
  res <- data[1L, group_vars(data), drop = FALSE]
  res <- suppressMessages(semi_join(get_group_details(data), res))
  unlist(res[, ".rows"])
}

#' @description
#' * `cur_column()` gives the name of the current column (in [across()] only).
#' @rdname context
#' @export
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
