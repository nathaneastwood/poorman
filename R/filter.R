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
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  eval_env$env <- parent.frame()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  rows <- lapply(
    seq_along(conditions),
    function(i, frame) filter_rows(eval(conditions[[i]], context$.data, frame), conditions[[i]], i, nrow(.data)),
    frame = eval_env$env
  )
  rows <- Reduce("&", rows)
  .data[rows & !is.na(rows), , drop = FALSE]
}

#' @export
filter.grouped_df <- function(.data, ..., .preserve = FALSE) {
  # Only report a deprecated data frame result once rather than for every group
  seen <- character()
  res <- withCallingHandlers(
    apply_grouped_function(.data, function(df) filter(df, ...), .drop = TRUE, .reorder = TRUE),
    poorman_filter_df_warning = function(w) {
      if (conditionMessage(w) %in% seen) invokeRestart("muffleWarning")
      seen <<- c(seen, conditionMessage(w))
    }
  )

  groups <- group_vars(.data)
  pre_filtered_groups <- group_data(.data)
  post_filtered_groups <- calculate_groups(res, groups, drop = TRUE)

  if (!(!.preserve && isTRUE(attr(pre_filtered_groups, ".drop")))) {
    filtered_groups <- anti_join(pre_filtered_groups, post_filtered_groups, by = groups)
    filtered_groups <- filtered_groups[, groups, drop = FALSE]
    filtered_groups[[".rows"]] <- rep(list(integer()), length.out = nrow(filtered_groups))
    post_filtered_groups <- bind_rows(post_filtered_groups, filtered_groups)
    ordered <- do.call(order, unname(.subset(post_filtered_groups, groups)))
    post_filtered_groups <- post_filtered_groups[ordered, , drop = FALSE]
  }

  rownames(post_filtered_groups) <- NULL
  attr(post_filtered_groups, ".drop") <- group_by_drop_default(.data)
  attr(res, "groups") <- post_filtered_groups
  res
}

# -- Helpers -------------------------------------------------------------------

# As in dplyr, the columns of a data frame result, e.g. from `across()`, are combined with `&`, and matrix results
# must have a single column.
filter_rows <- function(res, cond, i, n) {
  if (is.data.frame(res)) {
    msg <- if (is.call(cond) && identical(cond[[1L]], as.name("across"))) {
      "Using `across()` in `filter()` is deprecated, use `if_any()` or `if_all()`."
    } else {
      "data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`."
    }
    warning(structure(
      class = c("poorman_filter_df_warning", "warning", "condition"),
      list(message = msg, call = NULL)
    ))
    return(Reduce("&", res, rep(TRUE, n)))
  }
  if (is.matrix(res)) {
    if (ncol(res) != 1L) {
      stop(
        sprintf("Problem while computing `..%s = %s`.\n", i, paste(deparse(cond), collapse = " ")),
        sprintf("x Input `..%s` must be a logical vector, not a %s[,%s].", i, typeof(res), ncol(res)),
        call. = FALSE
      )
    }
    res <- res[, 1L]
  }
  if (!is.logical(res)) {
    stop(
      sprintf("Problem while computing `..%s = %s`.\n", i, paste(deparse(cond), collapse = " ")),
      sprintf("x Input `..%s` must be a logical vector, not a %s.", i, if (is.object(res)) class(res)[1L] else typeof(res)),
      call. = FALSE
    )
  }
  res
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
