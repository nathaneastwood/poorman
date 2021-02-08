#' Determine the grouping structure of the data
#' @param x A `data.frame`
#' @param groups `character(n)`. The names of the grouping columns.
#' @return If `groups` is `NULL` or of length 0, then `x`; otherwise `x` with the attribute `"groups"`.
#' @noRd
groups_set <- function(x, groups) {
  attr(x, "groups") <- if (is.null(groups) || length(groups) == 0L) {
    NULL
  } else {
    compute_groups(x, groups)
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
    class(res) <- c("grouped_data", class(res))
    res <- groups_set(res, groups[groups %in% colnames(res)])
  }
  res
}

#' Compute the
#' @param .data A `data.frame`.
#' @param groups `character(n)`. The names of the groups.
#' @return
#' The columns give the values of the grouping variables. The last column, always called `.rows`, is a list of integer
#' vectors that gives the location of the rows in each group.
#' @noRd
compute_groups <- function(.data, groups) {
  class(.data) <- "data.frame"
  res <- unique(.data[, groups, drop = FALSE])
  nrow_res <- nrow(res)
  rows <- rep(list(NA), nrow_res)
  for (i in seq_len(nrow_res)) {
    rows[[i]] <- which(interaction(.data[, groups, drop = TRUE]) %in% interaction(res[i, groups]))
  }
  res$`.rows` <- rows
  res <- res[do.call(order, lapply(groups, function(x) res[, x])), , drop = FALSE]
  rownames(res) <- NULL
  res
}
