#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are `TRUE`.
#'
#' @param .data A `data.frame`.
#' @param ... Logical predicated defined in terms of the variables in `.data`. Multiple conditions are combined with
#' `&`. Arguments within `...` are automatically quoted and evaluated within the context of the `data.frame`.
#' @param .preserve `logical(1)`. Relevant when the .data input is grouped. If `.preserve = FALSE` (the default), the
#' grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#'
#' @section Useful filter functions:
#'
#' * `==`, `>`, `>=`, etc.
#' * `&`, `|`, `!`, `xor()`
#' * `is.na()`
#'
#' @examples
#' filter(mtcars, am == 1)
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#'
#' @return
#' A `data.frame`.
#'
#' @export
filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  conditions <- dotdotdot(...)
  if (length(conditions) == 0L) return(.data)
  check_filter(conditions, parent.frame())
  # As in dplyr, each group is given all of the conditions before the next group, so they are evaluated as one call
  # whose result is checked and combined into the rows to keep
  warned <- logical(length(conditions))
  keep_rows <- function(value, size, group, details) {
    keep <- TRUE
    for (i in seq_along(conditions)) {
      cond <- filter_rows(value[[i]], conditions[[i]], i, size, group, details, warn = !warned[[i]])
      warned[[i]] <<- warned[[i]] || is.data.frame(value[[i]])
      keep <- keep & cond
    }
    keep
  }
  res <- eval_groups(
    .data, list(conditions = as.call(c(list(base::list), unname(conditions)))), parent.frame(),
    group_size = TRUE, check = keep_rows
  )
  keep <- combine_groups(res$values[[1L]], res$sizes, "")
  keep[is.na(keep)] <- FALSE
  order <- ungroup_order(res$rows, .row_names_info(.data, 2L))
  if (!is.null(order)) keep <- keep[order]
  out <- subset_rows(.data, which(keep))
  if (!is.null(res$details)) attr(out, "groups") <- filter_groups(res$details, keep, .preserve)
  out
}

# -- Helpers -------------------------------------------------------------------

#' Find the groups that remain after `filter()`
#'
#' The keys of the groups do not change, as `filter()` keeps the rows in their order, so only the rows of each group
#' are found again, as positions in the result. Groups left empty are dropped unless `.preserve = TRUE` or the groups
#' were made with `.drop = FALSE`.
#'
#' @param details A `data.frame` of the groups of the input, as returned by `get_groups()`.
#' @param keep `logical(n)`. Whether each row of the input is kept, with no missing values.
#' @param .preserve `logical(1)`. Should empty groups be kept?
#' @return `details`, with the `.rows` of the result and without the groups that were dropped.
#' @noRd
filter_groups <- function(details, keep, .preserve) {
  rows <- details[[".rows"]]
  idx <- unlist(rows, use.names = FALSE)
  kept <- keep[idx]
  group <- rep.int(seq_along(rows), lengths(rows))[kept]
  group <- structure(group, levels = as.character(seq_along(rows)), class = "factor")
  rows <- unname(split.default(cumsum(keep)[idx[kept]], group))
  drop <- attr(details, ".drop")
  details[[".rows"]] <- rows
  if (!.preserve && isTRUE(drop) && any(lengths(rows) == 0L)) {
    details <- details[lengths(rows) > 0L, , drop = FALSE]
    rownames(details) <- NULL
    attr(details, ".drop") <- drop
  }
  details
}

#' Check the result of a condition and find the rows to keep
#'
#' As in dplyr, the result must have the size of its group or size one, which is checked before its type. The columns
#' of a data frame result, e.g. from `across()`, are each checked and then combined with `&`, and a matrix result must
#' have a single column.
#'
#' @param res The result of the condition for one group.
#' @param cond The unevaluated condition, for error messages.
#' @param i `integer(1)`. The position of the condition in the `...` of `filter()`.
#' @param size `integer(1)`. The size of the group.
#' @param group `integer(1)`. The position of the group in `details`, or `NULL` if the data is not grouped.
#' @param details A `data.frame` of the groups, as returned by `get_groups()`, or `NULL`.
#' @param warn `logical(1)`. Should a data frame result give the deprecation warning? It is given once for each
#' condition, rather than once for each group.
#' @return A `logical` vector of size `size` or one, which may contain missing values.
#' @noRd
filter_rows <- function(res, cond, i, size, group, details, warn) {
  res_size <- NROW(res)
  if (res_size != size && res_size != 1L) {
    expected <- if (size == 1L) "size 1" else sprintf("size %s or 1", size)
    stop_filter(cond, i, group, details, sprintf("x Input `..%s` must be of %s, not size %s.", i, expected, res_size))
  }
  if (!is.data.frame(res)) {
    check_filter_type(res, paste0("..", i), cond, i, group, details)
    return(if (is.matrix(res)) res[, 1L] else res)
  }
  for (j in names(res)) check_filter_type(res[[j]], sprintf("..%s$%s", i, j), cond, i, group, details)
  if (warn) {
    msg <- if (is.call(cond) && identical(cond[[1L]], as.name("across"))) {
      "Using `across()` in `filter()` is deprecated, use `if_any()` or `if_all()`."
    } else {
      "data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`."
    }
    warning(msg, call. = FALSE)
  }
  Reduce("&", res, rep(TRUE, res_size))
}

#' Check that the result of a condition is logical
#'
#' @param x The result of a condition, or a column of a data frame result.
#' @param name `character(1)`. The name of `x` in the error message, e.g. `..1` or `..1$a`.
#' @inheritParams filter_rows
#' @return `NULL`, invisibly, if `x` is a logical vector or a one column logical matrix; otherwise an error naming
#' the type of `x`, e.g. `integer`, `factor` or `logical[,2]`.
#' @noRd
check_filter_type <- function(x, name, cond, i, group, details) {
  if (is.logical(x) && (!is.matrix(x) || ncol(x) == 1L)) return(invisible())
  type <- if (is.matrix(x)) sprintf("%s[,%s]", typeof(x), ncol(x)) else if (is.object(x)) class(x)[1L] else typeof(x)
  stop_filter(cond, i, group, details, sprintf("x Input `%s` must be a logical vector, not a %s.", name, type))
}

#' Give the error for an invalid condition
#'
#' The error names the condition by its position and expression and, on grouped data, the group where it occurred.
#'
#' @param msg `character(1)`. The line which describes the problem, e.g. "x Input `..1` must be of size 1, not size
#' 2.".
#' @inheritParams filter_rows
#' @return Does not return; gives an error.
#' @noRd
stop_filter <- function(cond, i, group, details, msg) {
  stop(
    sprintf("Problem while computing `..%s = %s`.\n", i, paste(deparse(cond), collapse = " ")),
    msg,
    error_group(group, details),
    call. = FALSE
  )
}

#' Check that no condition is named
#'
#' As in dplyr, a named condition is taken to be a mistake for `==`, e.g. `filter(df, x = 1)`, unless it is a logical
#' constant such as `a = TRUE`. The error suggests `==` for the first named condition, labelled as `auto_label()`
#' labels an expression, so a long one is abbreviated, e.g. `x == c(...)`.
#'
#' @param conditions A `list` of unevaluated conditions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables for the label, e.g. the subscript of `.data[[var]]`.
#' @return `NULL`, invisibly, if no condition is named; otherwise an error.
#' @noRd
check_filter <- function(conditions, env) {
  named <- have_name(conditions) & !vapply(conditions, is.logical, NA)
  if (!any(named)) return(invisible())
  i <- which(named)[[1L]]
  stop(
    "We detected a named input.\n",
    "i This usually means that you've used `=` instead of `==`.\n",
    sprintf("i Did you mean `%s == %s`?", names(conditions)[[i]], auto_label(conditions[[i]], env = env)),
    call. = FALSE
  )
}
