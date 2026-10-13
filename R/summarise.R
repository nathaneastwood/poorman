#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarising the variables of an existing `data.frame`. Grouped `data.frame`s will
#' result in one row in the output for each group.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result.
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
  fns <- dotdotdot(...)
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  groups_exist <- context$is_grouped()
  if (groups_exist) {
    id <- attr(context$.data, "group_id", exact = TRUE)
    group <- if (is.null(id) || is.na(id)) {
      unique(context$get_columns(group_vars(context$.data)))
    } else {
      get_group_details(context$.data)[id, group_vars(context$.data), drop = FALSE]
    }
  }
  if (is_empty_list(fns)) {
    if (groups_exist) return(group) else return(data.frame())
  }
  res <- vector(mode = "list", length = length(fns))
  eval_env <- c(as.list(context$.data), vector(mode = "list", length = length(fns)))
  new_pos <- seq(length(context$.data) + 1L, length(eval_env), 1L)
  for (i in seq_along(fns)) {
    eval_env[[new_pos[i]]] <- do.call(with, list(eval_env, fns[[i]]))
    nms <- if (!is_named(eval_env[[new_pos[i]]])) {
      if (!is.null(names(fns)[[i]])) names(fns)[[i]] else deparse(fns[[i]])
    } else {
      NULL
    }
    if (!is.null(nms)) names(eval_env)[[new_pos[i]]] <- nms
    res[i] <- list(nms)
  }
  values <- eval_env[new_pos]
  if (all(vapply(values, is_simple_summary, NA))) {
    return(simple_summary(values, if (groups_exist) group))
  }
  res <- lapply(seq_along(values), function(i) build_data_frame(values[[i]], nms = res[[i]]))
  res <- do.call(cbind, res)
  if (groups_exist) {
    if (nrow(group) == 0L) res <- res[0L, , drop = FALSE]
    res <- cbind(group, res, row.names = NULL)
  }
  res
}

#' @export
summarise.grouped_df <- function(.data, ..., .groups = NULL) {
  if (!is.null(.groups)) {
    .groups <- match.arg(arg = .groups, choices = c("drop", "drop_last", "keep"), several.ok = FALSE)
  }

  groups <- group_vars(.data)
  res <- apply_grouped_function(.fn = "summarise", .data = .data, .drop = FALSE, ...)

  verbose <- summarise_verbose(.groups)

  if (is.null(.groups)) {
    .groups <- if (anyDuplicated(group_ids(.subset(res, groups)))) "keep" else "drop_last"
  }

  if (.groups == "drop_last") {
    n <- length(groups)
    if (n > 1) {
      if (verbose) summarise_inform(groups[-n])
      res <- groups_set(res, groups[-n], group_by_drop_default(.data))
    }
  } else if (.groups == "keep") {
    if (verbose) summarise_inform(groups)
    res <- groups_set(res, groups, group_by_drop_default(.data))
  } else if (.groups == "drop") {
    res <- ungroup(res)
  }

  rownames(res) <- NULL
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

# An unnamed atomic vector without dimensions; `build_data_frame()` would turn it into a single column `data.frame`.
is_simple_summary <- function(x) {
  is.atomic(x) && length(x) > 0L && is.null(dim(x)) && is.null(names(x)) &&
    !(is.character(x) && isTRUE(getOption("stringsAsFactors")))
}

# A faster equivalent of `cbind(group, do.call(cbind, lapply(x, build_data_frame)), row.names = NULL)` for results
# which all pass `is_simple_summary()` and have the same length.
simple_summary <- function(x, group = NULL) {
  n <- unique(lengths(x))
  if (length(n) != 1L || anyDuplicated(names(x)) || (!is.null(group) && !nrow(group) %in% c(1L, n))) {
    res <- do.call(cbind, lapply(names(x), function(nm) build_data_frame(x[[nm]], nms = nm)))
    if (!is.null(group)) {
      if (nrow(group) == 0L) res <- res[0L, , drop = FALSE]
      res <- cbind(group, res, row.names = NULL)
    }
    return(res)
  }
  if (!is.null(group)) {
    group <- lapply(group, function(col) if (length(col) == n) col else rep(col, length.out = n))
    x <- c(group, x)
  }
  structure(x, class = "data.frame", row.names = .set_row_names(n))
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
