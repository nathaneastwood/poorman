#' Apply a function (or functions) across multiple columns
#'
#' @description
#' `across()` makes it easy to apply the same transformation to multiple columns, allowing you to use [select()]
#' semantics inside in "data-masking" functions like [summarise()] and [mutate()].
#'
#' `if_any()` and `if_all()` are used to apply the same predicate function to a selection of columns and combine the
#' results into a single logical vector.
#'
#' `across()` supersedes the family of `dplyr` "scoped variants" like `summarise_at()`, `summarise_if()`, and
#' `summarise_all()` and therefore these functions will not be implemented in `poorman`.
#'
#' @param cols,.cols <[`poor-select`][select_helpers]> Columns to transform. Because `across()` is used within functions
#' like `summarise()` and `mutate()`, you can't select or compute upon grouping variables.
#' @param .fns Functions to apply to each of the selected columns.
#' Possible values are:
#'
#' - `NULL`, to returns the columns untransformed.
#' - A function, e.g. `mean`.
#' - A lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#' - A list of functions/lambdas, e.g. `list(mean = mean, n_miss = ~ sum(is.na(.x))`
#'
#' Within these functions you can use [cur_column()] and [cur_group()] to access the current column and grouping keys
#' respectively.
#' @param ... Additional arguments for the function calls in `.fns`.
#' @param .names A gluestick specification that describes how to name the output
#'  columns. This can use `{.col}` to stand for the selected column name, and `{.fn}`
#'  to stand for the name of the function being applied. The default (`NULL`) is
#'  equivalent to "`{.col}`" for the single function case and "`{.col}_{.fn}`" for
#'  the case where a list is used for `.fns`.
#'
#' @return
#' `across()` returns a `data.frame` with one column for each column in `.cols` and each function in `.fns`.
#'
#' `if_any()` and `if_all()` return a logical vector.
#'
#' @examples
#' # across() -----------------------------------------------------------------
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean))
#' iris %>%
#'   mutate(across(where(is.factor), as.character))
#'
#' # Additional parameters can be passed to functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, na.rm = TRUE))
#'
#' # A named list of functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))
#'
#' # Use the .names argument to control the output names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"),
#'     mean,
#'     .names = c("mean_sepal_length", "mean_sepal_width"))
#'   )
#'
#' # if_any() and if_all() ----------------------------------------------------
#' iris %>%
#'   filter(if_any(ends_with("Width"), ~ . > 4))
#' iris %>%
#'   filter(if_all(ends_with("Width"), ~ . > 2))
#'
#' @export
across <- function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
  across_columns(substitute(.cols), .fns, ..., .names = .names, .env = parent.frame(), .call = sys.call())
}

#' @rdname across
#' @export
if_any <- function(.cols, .fns = NULL, ..., .names = NULL) {
  df <- across_columns(substitute(.cols), .fns, ..., .names = .names, .env = parent.frame(), .call = sys.call())
  # As in dplyr, no columns give TRUE, so `filter()` keeps every row, rather than the FALSE of `any()`
  if (ncol(df) == 0L) return(TRUE)
  check_if_types(df)
  Reduce(`|`, df)
}

#' @rdname across
#' @export
if_all <- function(.cols, .fns = NULL, ..., .names = NULL) {
  df <- across_columns(substitute(.cols), .fns, ..., .names = .names, .env = parent.frame(), .call = sys.call())
  if (ncol(df) == 0L) return(TRUE)
  check_if_types(df)
  Reduce(`&`, df)
}

check_if_types <- function(df) {
  types <- vapply(df, class, NA_character_)
  not_logical <- types != "logical"
  if (any(not_logical)) {
    stop(
      "Cannot convert the following columns to <logical>:\n    ",
      paste0(colnames(df)[not_logical], " <", types, "> ", collapse = "\n    ")
    )
  }
}

# -- helpers -----------------------------------------------------------------------------------------------------------

#' Apply functions across columns, the body of `across()`, `if_any()` and `if_all()`
#'
#' @param .cols The unevaluated selection of columns.
#' @param .fns,...,.names As in `across()`.
#' @param .env The `environment` in which `across()` was called, where the objects that the selection uses are found,
#' e.g. a vector of names given to `all_of()` or a predicate given to `where()`.
#' @param .call The call to `across()`, `if_any()` or `if_all()`, which identifies it amongst the calls of the
#' expression.
#' @return A `data.frame` with a column for each selected column and each function.
#' @noRd
across_columns <- function(.cols, .fns, ..., .names, .env, .call) {
  setup <- across_setup(.cols, .fns, .names, .env, .call)
  # The functions are the same objects in every group unless they are made in the expression, e.g. a lambda, which
  # must then be taken from each group so that it sees the columns of its group
  funs <- if (identical(.fns, setup$fns)) setup$funs else across_funs(.fns)
  cols <- setup$cols
  n_cols <- length(cols)
  # As in dplyr, no columns still give a data frame with a row for each row of the group, e.g. so that `filter()` keeps
  # every row
  if (n_cols == 0L) {
    return(structure(list(), names = character(), row.names = .set_row_names(context$get_nrow()), class = "data.frame"))
  }
  names <- setup$names
  if (is.null(funs)) {
    data <- data.frame(context$get_columns(cols))
    if (is.null(names)) {
      return(data)
    } else {
      return(setNames(data, names))
    }
  }
  # Within a grouped verb the columns are read from the data mask of the group, which is faster than building `.data`
  mask <- context$mask
  data <- if (is.null(mask)) as.list(context$get_columns(cols)) else mget(cols, envir = mask, inherits = FALSE)
  n_fns <- length(funs)
  res <- vector(mode = "list", length = n_fns * n_cols)
  k <- 1L
  for (i in seq_len(n_cols)) {
    context$cur_column <- cols[[i]]
    col <- data[[i]]
    for (j in seq_len(n_fns)) {
      res[[k]] <- funs[[j]](col, ...)
      k <- k + 1L
    }
  }
  names(res) <- names
  across_data_frame(res, setup$plain_names)
}

#' Find the columns of an `across()` call and the names of its results
#'
#' Within a grouped verb, the selection is made in the first group that the call is evaluated in and kept in
#' `context$across_setups` for the other groups of the expression. As in dplyr, the columns are therefore the same in
#' every group, and `where()` tests the full columns rather than those of one group. A call is identified by its
#' expression, the environment it is evaluated in, the number and names of its functions and `.names`; the environment
#' of a call evaluated directly in the expression is the data mask of each group, which stands for all of them.
#'
#' @param .cols,.fns,.names,.env,.call As in `across_columns()`.
#' @return A `list` with `cols`, the names of the selected columns, `names`, the names of the results, `plain_names`,
#' whether `as.data.frame()` would keep those names as they are, and `fns` and `funs`, the `.fns` that the setup was
#' made with and its functions, as returned by `across_funs()`.
#' @noRd
across_setup <- function(.cols, .fns, .names, .env, .call) {
  mask <- context$mask
  key <- list(
    call = .call,
    env = if (!is.null(mask) && identical(.env, mask)) NULL else .env,
    # The names of the results depend on how many functions there are and on their names, not on the functions
    fns = if (is.list(.fns)) list(length(.fns), names(.fns)) else is.null(.fns),
    names = .names
  )
  for (setup in context$across_setups) if (identical(setup$key, key)) return(setup)
  cols <- across_select(.cols, .env)
  funs <- across_funs(.fns)
  names <- across_names(cols, funs, .names)
  setup <- list(
    key = key, cols = cols, names = names,
    plain_names = !is.null(names) && identical(make.names(names, unique = TRUE), names), fns = .fns, funs = funs
  )
  if (!is.null(mask)) context$across_setups <- c(context$across_setups, list(setup))
  setup
}

#' Select the columns of an `across()` call
#'
#' @param .cols,.env As in `across_columns()`.
#' @return `character(n)`. The names of the selected columns, without the grouping columns.
#' @noRd
across_select <- function(.cols, .env) {
  cols <- eval_select_pos(
    .data = context$.data, .cols, .group_pos = FALSE, .env = .env, .values = context$full_data
  )
  cols <- context$get_colnames()[cols]
  if (context$is_grouped()) cols <- setdiff(cols, group_vars(context$.data))
  cols
}

#' Turn the `.fns` of `across()` into a named `list` of functions
#'
#' @param .fns As in `across()`.
#' @return A `list` of functions named by their names in `.fns`, or by their position where they have none, or `NULL`
#' if `.fns` is `NULL`.
#' @noRd
across_funs <- function(.fns) {
  if (is.null(.fns)) return(NULL)
  funs <- if (!is.list(.fns)) list(.fns) else .fns
  f_nms <- names(funs)
  if (is.null(f_nms)) {
    names(funs) <- seq_along(funs)
    # A single unnamed function names its results after the columns alone
    attr(funs, "unnamed") <- TRUE
  } else if (any(nchar(f_nms) == 0L)) {
    miss <- which(nchar(f_nms) == 0L)
    names(funs)[miss] <- miss
  }
  structure(lapply(funs, as_function), unnamed = attr(funs, "unnamed"))
}

#' Name the results of an `across()` call
#'
#' @param cols `character(n)`. The selected columns.
#' @param funs The functions, as returned by `across_funs()`.
#' @param .names As in `across()`.
#' @return `character()` with the name of each result, or `NULL` to keep the names of the columns.
#' @noRd
across_names <- function(cols, funs, .names) {
  if (is.null(funs)) return(.names)
  if (!is.null(.names)) return(fill_names_template(.names, cols, unique(names(funs))))
  if (length(funs) == 1L && isTRUE(attr(funs, "unnamed"))) return(cols)
  nms <- do.call(paste, c(rev(expand.grid(names(funs), cols)), sep = "_"))
  if (length(nms) == 0L) NULL else nms
}

#' Fill in a `.names` template of `across()`
#'
#' @param .names As in `across()`.
#' @param cols `character(n)`. The selected columns.
#' @param fn_names `character()`. The names of the functions.
#' @return `character()`. The name of each result, or `.names` as it is if it is not a template using `{.col}` or
#' `{.fn}`.
#' @noRd
fill_names_template <- function(.names, cols, fn_names) {
  if (length(.names) != 1L || !grepl("\\{\\.col\\}|\\{\\.fn\\}", .names) || length(cols) == 0L) return(.names)
  # gluestick() fills in every name at once, as `.col` and `.fn` are vectors
  gluestick(.names, src = list(.col = rep(cols, each = length(fn_names)), .fn = rep(fn_names, times = length(cols))))
}

# A faster equivalent of `as.data.frame(x)` for the common case where each result is a plain vector of the same
# length, with names which `as.data.frame()` would leave unchanged (`plain_names`, found once for each expression by
# `across_setup()`). Anything else falls back to `as.data.frame()`.
across_data_frame <- function(x, plain_names) {
  if (!plain_names || !all(vapply(x, is_bare_atomic, NA))) return(as.data.frame(x))
  n <- length(x[[1L]])
  if (!all(lengths(x) == n)) return(as.data.frame(x))
  attr(x, "row.names") <- .set_row_names(n)
  class(x) <- "data.frame"
  x
}
