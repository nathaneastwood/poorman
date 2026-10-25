#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarising the variables of an existing `data.frame`. Grouped `data.frame`s will
#' result in one row in the output for each group.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result.
#'
#' A name can also be given with `:=`, e.g. `total := sum(x)`. A string on the left of `:=` is a `gluestick` template
#' which is filled in from the calling environment, so that a name can be built from a variable, e.g.
#' `"{var}_mean" := mean(x)`.
#' @param .groups `character(1)`. Grouping structure of the result.
#'
#' * `"drop_last"`: drops the last level of grouping.
#' * `"drop"`: all levels of grouping are dropped.
#' * `"keep"`: keeps the same grouping structure as `.data`.
#'
#' When `.groups` is not specified, it is chosen based on the number of rows of the results:
#' * If all the results have 1 row, you get `"drop_last"`.
#' * If the number of rows varies, you get `"keep"`.
#'
#' In addition, a message informs you of that choice, unless the result is ungrouped, the option
#' `"poorman.summarise.inform"` is set to `FALSE`.
#'
#' The value can be:
#' * A vector of length `1`, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#' * A vector of length `n`, e.g. `quantile()`.
#'
#' @section Automatic names:
#' An unnamed expression is named after its code, e.g. `mean(disp)`, and `.data$disp` is named `disp`. When the code
#' is too long to read as a name, it is abbreviated with `...`. A call keeps its function, e.g. `mean(...)`, and an
#' operator keeps as many of its two sides as fit in 60 characters, e.g. `... %>% select(a, b)`.
#'
#' The names are those of `dplyr`, with these differences:
#' * The limit of 60 characters is fixed. `dplyr` keeps some names of up to 63 characters whose right side is a call,
#'   such as a long name followed by `+ g()`.
#' * Characters are counted rather than bytes, so names with accented or other multibyte characters are not
#'   abbreviated sooner.
#' * `?` and `:=` are treated as calls rather than operators, e.g. `` `?`(...) ``, as R prints them.
#'
#' @inheritSection mutate Variables made inside expressions
#'
#' @examples
#' # A summary applied to ungrouped tbl returns a single row
#' mtcars %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # Usually, you'll want to group first
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # You can summarise to more than one value:
#' mtcars %>%
#'    group_by(cyl) %>%
#'    summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#'
#' # You use a data frame to create multiple columns so you can wrap
#' # this up into a function:
#' my_quantile <- function(x, probs) {
#'   data.frame(x = quantile(x, probs), probs = probs)
#' }
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(my_quantile(disp, c(0.25, 0.75)))
#'
#' # Each summary call removes one grouping level (since that group
#' # is now just a single row)
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   summarise(cyl_n = n()) %>%
#'   group_vars()
#'
#' @name summarise
#' @export
summarise <- function(.data, ..., .groups = NULL) {
  UseMethod("summarise")
}

#' @export
summarise.data.frame <- function(.data, ..., .groups = NULL) {
  summarise_groups(.data, dotdotdot(...), parent.frame())$data
}

#' @export
summarise.grouped_df <- function(.data, ..., .groups = NULL) {
  if (!is.null(.groups)) {
    .groups <- match.arg(arg = .groups, choices = c("drop", "drop_last", "keep"), several.ok = FALSE)
  }

  groups <- group_vars(.data)
  res <- summarise_groups(.data, dotdotdot(...), parent.frame())

  verbose <- summarise_verbose(.groups)

  # As in dplyr, the grouping is kept when any group gave other than one row
  if (is.null(.groups)) .groups <- if (res$all_one) "drop_last" else "keep"
  res <- res$data

  if (.groups == "drop_last") {
    n <- length(groups)
    if (n > 1) {
      if (verbose) summarise_inform(groups[-n])
      res <- summarise_regroup(res, groups[-n], group_by_drop_default(.data))
    }
  } else if (.groups == "keep") {
    if (verbose) summarise_inform(groups)
    res <- summarise_regroup(res, groups, group_by_drop_default(.data))
  }

  res
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @export
summarize.data.frame <- summarise.data.frame
#' @export
summarize.grouped_df <- summarise.grouped_df

# -- Helpers -------------------------------------------------------------------

#' Evaluate summary expressions for each group
#'
#' The expressions are evaluated by `eval_groups()` and the results of each are combined across the groups by
#' `combine_groups()`.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with `data`, the result as a `data.frame` (the grouping columns followed by the summaries), and
#' `all_one`, whether every group gave exactly one row.
#' @noRd
summarise_groups <- function(.data, dots, env) {
  res <- eval_groups(.data, dots, env, common_size_rule)
  grouped <- length(res$groups) > 0L
  keys <- if (grouped) as.list(res$details)[res$groups]
  sizes <- res$sizes
  # As in dplyr, data with no groups counts as every group giving one row
  all_one <- res$empty || all(sizes == 1L)
  if (res$empty) sizes <- 0L

  out <- list()
  for (i in seq_along(dots)) {
    col <- combine_groups(res, i, sizes)
    if (is.null(col)) next
    if (is.data.frame(col)) attr(col, "row.names") <- .set_row_names(nrow(col))
    if (res$unnamed[[i]] && is.data.frame(col)) {
      for (j in names(col)) out[[j]] <- .subset2(col, j)
    } else {
      out[[res$labels[[i]]]] <- col
    }
  }

  res <- summarise_result(keys, out, sizes, grouped)
  res$all_one <- all_one
  res
}

# Join the grouping columns, repeated for each row of their group, to the summaries. As in dplyr, a summary with the
# name of a grouping column replaces it.
summarise_result <- function(keys, cols, sizes, grouped) {
  if (!grouped && length(cols) == 0L) return(list(data = empty_summary(), all_one = TRUE))
  all_one <- all(sizes == 1L)
  if (!all_one) {
    idx <- rep.int(seq_along(sizes), sizes)
    keys <- lapply(keys, function(key) key[idx])
  }
  for (nm in names(cols)) keys[[nm]] <- cols[[nm]]
  if (is.null(keys)) keys <- list()
  n <- if (grouped) sum(sizes) else sizes[[1L]]
  attributes(keys) <- list(names = as.character(names(keys)), class = "data.frame", row.names = .set_row_names(n))
  list(data = keys, all_one = all_one)
}

summarise_regroup <- function(x, groups, drop) {
  class(x) <- c("grouped_df", "data.frame")
  groups_set(x, groups, drop)
}

# As in dplyr, a summary with no columns has one row
empty_summary <- function() {
  structure(list(), names = character(0), row.names = .set_row_names(1L), class = "data.frame")
}

summarise_inform <- function(new_groups) {
  message(sprintf(
    "`summarise()` has grouped output by %s. You can override using the `.groups` argument.",
    paste0("'", new_groups, "'", collapse = ", ")
  ))
}

summarise_verbose <- function(.groups) {
  is.null(.groups) &&
    !identical(getOption("poorman.summarise.inform"), FALSE)
}
