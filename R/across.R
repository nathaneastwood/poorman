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
#' like `summarise()` and `mutate()`, you can't select or compute upon grouping variables: positions count the other
#' columns, and `where()` does not test the grouping variables. Unlike `dplyr` 1.0.10, where `group_cols()` gives an
#' error within `across()`, `group_cols()` selects no columns, so `-group_cols()` selects every column.
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
#'
#' `.fns` is evaluated in each group, as any other code of the expression is, so it can depend on the group, e.g.
#' `if (cur_group_id() == 1L) min else max`. This differs from `dplyr` 1.0.10, which evaluates the `.fns` of a call
#' made directly in the verb once, before any group, where `cur_group_id()` gives `0`, but evaluates the `.fns` of a
#' call made from a function in each group.
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
  funs <- if (identical(.fns, setup$fns)) setup$funs else across_group_funs(.fns, setup)
  cols <- setup$cols
  n_cols <- length(cols)
  if (n_cols == 0L) {
    return(new_data_frame(list(), context$get_nrow()))
  }
  names <- setup$names
  if (is.null(funs)) {
    # The columns are taken as they are, as `data.frame()` would repair names which are not syntactic
    data <- context$get_columns(cols)
    data <- new_data_frame(as.list(data), nrow(data))
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
  across_data_frame(res)
}

#' Find the columns of an `across()` call and the names of its results
#'
#' Within a grouped verb, the selection is made in the first group that the call is evaluated in and kept in
#' `context$across_setups` for the other groups of the expression. The columns are therefore the same in
#' every group, and `where()` tests the full columns rather than those of one group. A call is identified by its
#' expression, the number and names of its functions and `.names`. Only calls evaluated directly in the expression,
#' whose environment is the data mask of each group, are kept; a call made from a function is set up each time.
#'
#' @param .cols,.fns,.names,.env,.call As in `across_columns()`.
#' @return A `list` with `cols`, the names of the selected columns, `names`, the names of the results, and `fns` and
#' `funs`, the `.fns` that the setup was made with and its functions, as returned by `across_funs()`.
#' @noRd
across_setup <- function(.cols, .fns, .names, .env, .call) {
  mask <- context$mask
  # Only a call made directly in the expression is kept. A call made from a function, e.g. a helper or a loop within
  # one, is evaluated in a new frame in each group, or in the same frame with objects that change between calls, so
  # its selection is made again each time
  cached <- !is.null(mask) && identical(.env, mask)
  key <- list(
    call = .call,
    # The names of the results depend on how many functions there are and on their names, not on the functions
    fns = if (is.list(.fns)) list(length(.fns), names(.fns)) else is.null(.fns),
    names = .names
  )
  if (cached) for (setup in context$across_setups) if (identical(setup$key, key)) return(setup)
  cols <- across_select(.cols, .env)
  funs <- across_funs(.fns)
  if (!is.null(funs) && length(funs) == 0L) cols <- character()
  names <- across_names(cols, funs, .names)
  check_unique_names(names)
  setup <- list(key = key, cols = cols, names = names, fns = .fns, funs = funs)
  if (cached) context$across_setups <- c(context$across_setups, list(setup))
  setup
}

#' Select the columns of an `across()` call
#'
#' @param .cols,.env As in `across_columns()`.
#' @return `character(n)`. The names of the selected columns, which are never grouping columns.
#' @noRd
across_select <- function(.cols, .env) {
  # Called directly in the expression, `across()` is evaluated in the data mask of the group, where a grouping column
  # is a variable. The objects that the selection uses are found where the verb was called instead, so that a grouping
  # column is not selected by its values
  mask <- context$mask
  if (!is.null(mask) && identical(.env, mask)) .env <- parent.env(mask)
  data <- context$.data
  if (context$is_grouped()) {
    keep <- setdiff(names(data), group_vars(data))
    data <- new_data_frame(.subset(data, keep), nrow(data))
  }
  cols <- eval_select_pos(.data = data, .cols, .group_pos = FALSE, .env = .env, .values = context$full_data)
  names(data)[cols]
}

#' Turn the `.fns` of `across()` into a named `list` of functions
#'
#' @param .fns As in `across()`.
#' @return A `list` of functions named by their names in `.fns`, or by their position where they have none, or `NULL`
#' if `.fns` is `NULL`. Its `"single"` attribute is `TRUE` if `.fns` is one function rather than a list.
#' @noRd
across_funs <- function(.fns) {
  if (is.null(.fns)) return(NULL)
  funs <- if (!is.list(.fns)) list(.fns) else .fns
  f_nms <- names(funs)
  if (is.null(f_nms)) {
    names(funs) <- seq_along(funs)
  } else if (any(nchar(f_nms) == 0L)) {
    miss <- which(nchar(f_nms) == 0L)
    names(funs)[miss] <- miss
  }
  structure(lapply(funs, as_function), single = !is.list(.fns))
}

#' Turn the `.fns` of `across()` in a group into functions, reusing those of the setup
#'
#' A lambda written in the expression is a new formula in each group, whose environment is the data mask of the group.
#' Rather than building its function again, the function built from the same formula by `across_setup()` is given the
#' environment of the new formula. Any other function which differs from that of the setup is converted as usual.
#'
#' @param .fns As in `across()`.
#' @param setup The setup of the call, as returned by `across_setup()`, made with `.fns` of the same length and names.
#' @return As `across_funs()`.
#' @noRd
across_group_funs <- function(.fns, setup) {
  funs <- setup$funs
  if (is.null(.fns) || is.null(funs)) return(across_funs(.fns))
  fns <- if (is.list(.fns)) .fns else list(.fns)
  first <- if (is.list(setup$fns)) setup$fns else list(setup$fns)
  for (i in seq_along(fns)) {
    fn <- fns[[i]]
    if (identical(fn, first[[i]])) next
    if (is_same_lambda(fn, first[[i]])) {
      f <- funs[[i]]
      environment(f) <- attr(fn, ".Environment", exact = TRUE)
      funs[[i]] <- f
    } else {
      funs[[i]] <- as_function(fn)
    }
  }
  funs
}

#' Name the results of an `across()` call
#'
#' @param cols `character(n)`. The selected columns.
#' @param funs The functions, as returned by `across_funs()`.
#' @param .names As in `across()`.
#' @return `character()` with the name of each result, or `NULL` to keep the names of the columns.
#' @noRd
across_names <- function(cols, funs, .names) {
  if (is.null(funs)) return(fill_names_template(.names, cols, "1"))
  if (!is.null(.names)) return(fill_names_template(.names, cols, unique(names(funs))))
  if (isTRUE(attr(funs, "single"))) return(cols)
  nms <- do.call(paste, c(rev(expand.grid(names(funs), cols)), sep = "_"))
  if (length(nms) == 0L) NULL else nms
}

#' Check that the names of the results of an `across()` call are unique
#'
#' @param names `character()`. The names of the results, or `NULL`.
#' @return `NULL`, invisibly, or gives an error.
#' @noRd
check_unique_names <- function(names) {
  dups <- unique(names[duplicated(names)])
  if (length(dups) == 0L) return(invisible())
  locations <- vapply(dups, function(dup) {
    at <- which(names == dup)
    last <- length(at)
    if (last == 2L) paste(at, collapse = " and ") else paste0(paste(at[-last], collapse = ", "), ", and ", at[last])
  }, NA_character_)
  stop(
    "Names must be unique.\nx These names are duplicated:\n",
    paste0("  * \"", dups, "\" at locations ", locations, ".", collapse = "\n"),
    call. = FALSE
  )
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

#' Build the data frame of the results of an `across()` call
#'
#' Each result is one column, whatever its type: a list, data frame or matrix result is not split into
#' several columns as `as.data.frame()` would. Results of size one are recycled to the size of the others. The names
#' are kept as they are, even when they are not syntactic, e.g. `"mean of x"`.
#'
#' @param x A named `list` with the result of each function for each column.
#' @return A `data.frame` with a column for each result.
#' @noRd
across_data_frame <- function(x) {
  sizes <- vapply(x, NROW, 1L)
  n <- max(sizes)
  recycle <- which(sizes != n)
  # Results of other sizes give the error of `as.data.frame()`
  if (any(sizes[recycle] != 1L)) return(as.data.frame(x))
  x[recycle] <- lapply(x[recycle], vec_slice, rep(1L, n))
  new_data_frame(x, n)
}
