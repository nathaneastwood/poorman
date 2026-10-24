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
  check_filter(conditions)
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

# The rows of the groups that remain after `filter()`. The keys do not change, as the rows keep their order. Groups
# left empty are dropped unless `.preserve = TRUE` or the groups were made with `.drop = FALSE`.
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

# Check the result of a condition for a group of size `size` and give the rows to keep, a `logical` of size `size` or
# one. As in dplyr, the columns of a data frame result, e.g. from `across()`, are combined with `&`, and matrix results
# must have a single column. The size is checked before the type.
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

# Give an error unless `x`, the result of a condition or a column of it called `name`, is a logical vector or a one
# column logical matrix
check_filter_type <- function(x, name, cond, i, group, details) {
  if (is.logical(x) && (!is.matrix(x) || ncol(x) == 1L)) return(invisible())
  type <- if (is.matrix(x)) sprintf("%s[,%s]", typeof(x), ncol(x)) else if (is.object(x)) class(x)[1L] else typeof(x)
  stop_filter(cond, i, group, details, sprintf("x Input `%s` must be a logical vector, not a %s.", name, type))
}

stop_filter <- function(cond, i, group, details, msg) {
  stop(
    sprintf("Problem while computing `..%s = %s`.\n", i, paste(deparse(cond), collapse = " ")),
    msg,
    error_group(group, details),
    call. = FALSE
  )
}

check_filter <- function(conditions) {
  named <- have_name(conditions)
  for (i in which(named)) {
    if (!is.logical(conditions[[i]])) {
      stop(
        sprintf("Problem with `filter()` input `..%s`.\n", i),
        sprintf("Input `..%s` is named.\n", i),
        "This usually means that you've used `=` instead of `==`.\n",
        sprintf("Did you mean `%s == %s`?", names(conditions)[[i]], conditions[[i]])
      )
    }
  }
}
