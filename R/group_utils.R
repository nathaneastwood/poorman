#' Determine the grouping structure of the data
#' @param x A `data.frame`
#' @param groups `character(n)`. The names of the grouping columns.
#' @return If `groups` is `NULL` or of length 0, then `x`; otherwise `x` with the attribute `"groups"`.
#' @noRd
groups_set <- function(x, groups, drop = group_by_drop_default(x)) {
  attr(x, "groups") <- if (is.null(groups) || length(groups) == 0L) {
    NULL
  } else {
    calculate_groups(x, groups, drop)
  }
  x
}

#' Get group details
#' @param x A `data.frame`.
#' @return A `data.frame` containing the grouping columns where each row represents a single combination of groups and
#' a column called `.rows` which contains the row numbers each group combination appears on.
#' @noRd
get_group_details <- function(x) {
  groups <- attr(x, "groups", exact = TRUE)
  if (is.null(groups)) character(0) else groups
}

#' Check if an R object has groups
#' @return `logical(1)`
#' @noRd
has_groups <- function(x) {
  groups <- group_vars(x)
  if (length(groups) == 0L) FALSE else TRUE
}

#' @param fn `character(1)`. The function to apply to each group.
#' @param .data A `data.frame`.
#' @param ... Arguments to be passed to `fn`.
#' @noRd
apply_grouped_function <- function(fn, .data, drop = FALSE, ...) {
  groups <- group_vars(.data)
  grouped <- split_into_groups(.data, groups, drop)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  if (any(groups %in% colnames(res))) {
    class(res) <- c("grouped_df", class(res))
    res <- groups_set(res, groups[groups %in% colnames(res)])
  }
  res
}

#' Calculate the groups
#' @param data A `data.frame`.
#' @param groups `character(n)`. The names of the groups.
#' @return
#' The columns give the values of the grouping variables. The last column, always called `.rows`, is a list of integer
#' vectors that gives the location of the rows in each group.
#' @noRd
calculate_groups <- function(data, groups, drop = group_by_drop_default(data)) {
  data <- ungroup(data)

  unknown <- setdiff(groups, colnames(data))
  if (length(unknown) > 0L) {
    stop(sprintf("`groups` missing from `data`: %s.", paste0(groups, collapse = ", ")))
  }

  unique_groups <- unique(data[, groups, drop = FALSE])
  is_factor <- do.call(c, lapply(unique_groups, function(x) is.factor(x)))
  n_comb <- nrow(unique_groups)
  rows <- rep(list(NA), n_comb)
  data_groups <- interaction(data[, groups, drop = TRUE])
  for (i in seq_len(n_comb)) {
    rows[[i]] <- which(data_groups %in% interaction(unique_groups[i, groups]))
  }

  if (!isTRUE(drop) && any(is_factor)) {
    na_lvls <- do.call(
      expand.grid,
      lapply(unique_groups, function(x) if (is.factor(x)) levels(x)[!(levels(x) %in% x)] else NA)
    )
    unique_groups <- rbind(unique_groups, na_lvls)
    for (i in seq_len(nrow(na_lvls))) {
      rows[[length(rows) + 1]] <- integer(0)
    }
  }

  unique_groups[[".rows"]] <- rows
  unique_groups <- unique_groups[do.call(order, lapply(groups, function(x) unique_groups[, x])), , drop = FALSE]
  rownames(unique_groups) <- NULL
  structure(unique_groups, .drop = drop)
}

is.grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
