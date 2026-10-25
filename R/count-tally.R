#' Count observations by group
#'
#' `count()` lets you quickly count the unique values of one or more variables:
#' `df %>% count(a, b)` is roughly equivalent to
#' `df %>% group_by(a, b) %>% summarise(n = n())`.
#' `count()` is paired with `tally()`, a lower-level helper that is equivalent to `df %>% summarise(n = n())`. Supply
#' `wt` to perform weighted counts, switching the summary from from `n = n()` to `n = sum(wt)`.
#' `add_count()` and `add_tally()` are equivalent to `count()` and `tally()` but use `mutate()` instead of `summarise()`
#' so that they add a new column with group-wise counts.
#'
#' @param x A `data.frame`.
#' @param ... Variables to group by.
#' @param wt Frequency weights. If `NULL`, the default, the rows of each group are counted. Otherwise, the weighted
#' count of each group is the sum of the (non-missing) values of `wt`, which may be a column or an expression of
#' columns and variables of the calling environment, e.g. `wt = price * k`. A column named `n` is not used as a
#' weight unless it is given as `wt`.
#' @param sort `logical(1)`. If `TRUE`, will show the largest groups at the top.
#' @param name `character(1)`. The name of the new column in the output. If omitted, it will default to `n`. If there's
#' already a column called `n`, it will error, and require you to specify the name.
#'
#' @examples
#' # count() is a convenient way to get a sense of the distribution of
#' # values in a dataset
#' mtcars %>% count(cyl)
#' mtcars %>% count(cyl, sort = TRUE)
#' mtcars %>% count(cyl, am, sort = TRUE)
#' # Note that if the data are already grouped, count() adds an additional grouping variable
#' # which is removed afterwards
#' mtcars %>% group_by(gear) %>% count(cyl)
#'
#' # tally() is a lower-level function that assumes you've done the grouping
#' mtcars %>% tally()
#' mtcars %>% group_by(cyl) %>% tally()
#'
#' # both count() and tally() have add_ variants that work like mutate() instead of summarise
#' mtcars %>% add_count(cyl, wt = am)
#' mtcars %>% add_tally(wt = am)
#'
#' @return A `data.frame`. `count()` and `add_count()` have the same groups as the input.
#'
#' @importFrom stats setNames
#'
#' @export
count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  groups <- group_vars(x)
  drop <- group_by_drop_default(x)
  env <- parent.frame()
  if (!missing(...)) x <- group_by_caller(x, ..., .add = TRUE, .env = env)
  # `wt` is passed on unevaluated and `tally()` is called from the caller's environment, so `tally()` finds the
  # variables of `wt` where `count()` would
  res <- do.call(tally, list(x, substitute(wt), sort, name), envir = env)
  restore_groups(res, groups, drop)
}

#' @rdname count
#' @export
tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  name <- check_name(x, name)
  n <- count_expr(substitute(wt))
  old <- options(poorman.summarise.inform = FALSE)
  on.exit(options(old), add = TRUE)
  res <- do.call(summarise, setNames(list(x, n), c(".data", name)), envir = parent.frame())
  if (isTRUE(sort)) res <- do.call(arrange, list(res, call("desc", as.name(name))))
  rownames(res) <- NULL
  res
}

#' @export
#' @rdname count
add_count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  name <- check_name(x, name)
  env <- parent.frame()
  wt <- substitute(wt)
  if (missing(...)) return(do.call(add_tally, list(x, wt, sort, name), envir = env))
  groups <- group_vars(x)
  drop <- group_by_drop_default(x)
  x <- group_by_caller(x, ..., .add = TRUE, .env = env)
  res <- do.call(add_tally, list(x, wt, sort, name), envir = env)
  restore_groups(res, groups, drop)
}

#' @export
#' @rdname count
add_tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  name <- check_name(x, name)
  n <- count_expr(substitute(wt))
  res <- do.call(mutate, setNames(list(x, n), c(".data", name)), envir = parent.frame())

  if (isTRUE(sort)) {
    do.call(arrange, list(res, call("desc", as.name(name))))
  } else {
    res
  }
}

#' Build the expression which counts the rows of a group
#'
#' The expression is evaluated by `summarise()` or `mutate()` in the environment of the user, where `n` and `sum` may
#' be undefined, e.g. when poorman is not attached, or may be other objects. So the functions themselves are put in
#' the call, rather than their names.
#'
#' @param wt The unevaluated `wt` argument of the verb: `NULL` to count rows, or an expression of the weights.
#' @return A `call`, `n()` or `sum(<wt>, na.rm = TRUE)`.
#' @noRd
count_expr <- function(wt) {
  if (identical(wt, quote(n()))) {
    warning("`wt = n()` is deprecated\ni You can now omit the `wt` argument", call. = FALSE)
    wt <- NULL
  }
  if (is.null(wt)) as.call(list(n)) else as.call(list(sum, wt, na.rm = TRUE))
}

#' Restore the groups of the input of a counting verb
#'
#' `count()` and `add_count()` group by their `...` only to count, so their result has the groups of their input. The
#' groups are rebuilt from scratch, as the result may still carry the groups added for counting.
#'
#' @param res A `data.frame`, the result of the count.
#' @param groups A `character` vector of the grouping variables of the input.
#' @param drop `logical(1)`. The `.drop` setting of the input, as from `group_by_drop_default()`.
#' @return `res`, grouped by `groups` with `.drop = drop`, or ungrouped when `groups` is empty.
#' @noRd
restore_groups <- function(res, groups, drop) {
  res <- ungroup(res)
  if (length(groups) > 0L) res <- do.call(group_by, c(list(res), lapply(groups, as.name), list(.drop = drop)))
  res
}

check_name <- function(df, name) {
  if (is.null(name)) {
    if ("n" %in% colnames(df)) {
      stop(
        "Column 'n' is already present in output\n",
        "* Use `name = \"new_name\"` to pick a new name"
      )
    }
    return("n")
  }

  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single string")
  }

  name
}
