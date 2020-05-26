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
#' @param wt If omitted, will count the number of rows. If specified, will perform a "weighted" count by summing the
#' (non-missing) values of variable `wt`. If omitted, and column `n` exists, it will automatically be used as a
#' weighting variable, although you will have to specify `name` to provide a new name for the output.
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
#' @export
count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  groups <- get_groups(x)
  if (!missing(...)) x <- group_by(x, ..., .add = TRUE)
  wt <- deparse_var(wt)
  res <- do.call(tally, list(x, wt, sort, name))
  if (length(groups) > 0L) res <- do.call(group_by, list(res, as.name(groups)))
  res
}

#' @rdname count
#' @export
tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  name <- check_name(x, name)
  wt <- deparse_var(wt)
  res <- do.call(summarise, set_names(list(x, as.name(tally_n(x, wt))), c(".data", name)))
  res <- ungroup(res)
  if (isTRUE(sort)) res <- do.call(arrange, list(res, call("desc", as.name(name))))
  rownames(res) <- NULL
  res
}

#' @export
#' @rdname count
add_count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  name <- check_name(x, name)
  row_names <- rownames(x)
  wt <- deparse_var(wt)
  if (!missing(...)) x <- group_by(x, ..., .add = TRUE)
  res <- do.call(add_tally, list(x, wt, sort, name))
  res[row_names, ]
}

#' @export
#' @rdname count
add_tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  wt <- deparse_var(wt)
  n <- tally_n(x, wt)
  name <- check_name(x, name)
  res <- do.call(mutate, set_names(list(x, as.name(n)), c(".data", name)))

  if (isTRUE(sort)) {
    do.call(arrange, list(res, call("desc", as.name(name))))
  } else {
    res
  }
}

tally_n <- function(x, wt) {
  if (is.null(wt) && "n" %in% colnames(x)) {
    message("Using `n` as weighting variable")
    wt <- "n"
  }
  context$.data <- x
  on.exit(rm(list = ".data", envir = context), add = TRUE)
  if (is.null(wt)) {
    "n()"
  } else {
    paste0("sum(", wt, ", na.rm = TRUE)")
  }
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
