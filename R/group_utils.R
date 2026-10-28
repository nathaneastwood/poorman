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

#' Calculate the groups
#' @param data A `data.frame`.
#' @param groups `character(n)`. The names of the groups.
#' @return
#' The columns give the values of the grouping variables. The last column, always called `.rows`, is a list of integer
#' vectors that gives the location of the rows in each group.
#' @noRd
calculate_groups <- function(data, groups, drop = group_by_drop_default(data)) {
  force(drop)
  data <- ungroup(data)

  unknown <- setdiff(groups, colnames(data))
  if (length(unknown) > 0L) {
    stop(sprintf("`groups` missing from `data`: %s.", paste0(groups, collapse = ", ")))
  }

  ids <- group_ids(.subset(data, groups))
  first <- which(!duplicated(ids))
  unique_groups <- data[first, groups, drop = FALSE]
  rows <- split_rows(ids, length(first))

  if (!isTRUE(drop) && any(vapply(unique_groups, is.factor, NA))) {
    empty <- expand_empty_groups(unique_groups)
    unique_groups <- rbind(unique_groups, empty)
    rows <- c(rows, rep(list(integer()), nrow(empty)))
  }

  unique_groups[[".rows"]] <- rows
  unique_groups <- unique_groups[do.call(order, unname(.subset(unique_groups, groups))), , drop = FALSE]
  rownames(unique_groups) <- NULL
  structure(unique_groups, .drop = drop)
}

#' Get the grouping structure of the data
#'
#' Uses the `"groups"` attribute of `.data` if it still describes the data, otherwise the groups are recalculated. The
#' attribute can be out of date if, for example, the rows were reordered with `[`.
#'
#' @param .data A `data.frame`.
#' @param groups `character(n)`. The names of the groups.
#' @noRd
get_groups <- function(.data, groups = group_vars(.data)) {
  details <- attr(.data, "groups", exact = TRUE)
  if (is_valid_groups(.data, details, groups)) details else calculate_groups(.data, groups)
}

#' Check that a grouping structure still describes the data
#'
#' @param .data A `data.frame`.
#' @param details A `data.frame` of the groups, as returned by `calculate_groups()`, or `NULL`.
#' @param groups `character(n)`. The names of the grouping columns.
#' @return `logical(1)`. `TRUE` if `details` has the grouping columns `groups`, each row of `.data` is in exactly one
#' group and the key columns of `.data`, read through `.rows`, match the keys of `details`.
#' @noRd
is_valid_groups <- function(.data, details, groups) {
  if (!is.data.frame(details) || !identical(setdiff(names(details), ".rows"), groups)) return(FALSE)
  rows <- details[[".rows"]]
  idx <- unlist(rows, use.names = FALSE)
  n <- nrow(.data)
  # Every row must appear exactly once; tabulate() ignores missing and out of range values so they fail this too
  if (!is.numeric(idx) || length(idx) != n || any(tabulate(idx, n) != 1L)) return(FALSE)
  group <- group_of_rows(rows)
  for (col in groups) {
    if (!identical(unname(.subset2(.data, col)[idx]), unname(.subset2(details, col)[group]))) return(FALSE)
  }
  TRUE
}

#' Find the rows of each group
#'
#' @param .data A `data.frame`.
#' @return A `list` with:
#' * `groups`, the grouping columns, and `details`, the grouping structure of `.data` (`NULL` if it is not grouped);
#' * `grouped`, whether `.data` is grouped;
#' * `rows`, the rows of each group, or all of the rows as one group if `.data` is not grouped;
#' * `empty`, whether `.data` is grouped but has no groups;
#' * `numbered`, whether `.data` has groups, so that a group can be named by its position in `details`.
#' @noRd
group_layout <- function(.data) {
  groups <- group_vars(.data)
  grouped <- length(groups) > 0L
  details <- if (grouped) get_groups(.data, groups)
  rows <- if (grouped) details[[".rows"]] else list(seq_len(.row_names_info(.data, 2L)))
  empty <- length(rows) == 0L
  list(groups = groups, details = details, grouped = grouped, rows = rows, empty = empty, numbered = grouped && !empty)
}

#' Drop the groups left empty by a verb
#'
#' Empty groups are kept if the groups were made with `.drop = FALSE`.
#'
#' @param details A `data.frame` of the groups, as returned by `get_groups()`, with the `.rows` of the result.
#' @return `details`, without its empty groups unless its `".drop"` attribute is `FALSE`.
#' @noRd
drop_empty_groups <- function(details) {
  drop <- attr(details, ".drop")
  empty <- lengths(details[[".rows"]]) == 0L
  if (!isTRUE(drop) || !any(empty)) return(details)
  details <- details[!empty, , drop = FALSE]
  rownames(details) <- NULL
  attr(details, ".drop") <- drop
  details
}

#' Find the order that puts results in group order back into the order of the rows
#'
#' @param rows A `list` of the rows of each group.
#' @param n `integer(1)`. The number of rows.
#' @return `NULL` if the groups are already in the order of the rows, otherwise the position of each row in
#' `unlist(rows)`.
#' @noRd
ungroup_order <- function(rows, n) {
  # The rows of a single group are all the rows, in order
  if (length(rows) <= 1L) return(NULL)
  idx <- unlist(rows, use.names = FALSE)
  if (identical(idx, seq_len(n))) return(NULL)
  res <- integer(n)
  res[idx] <- seq_len(n)
  res
}

#' Split each column of a `data.frame` by group
#'
#' With few groups, indexing each group is fastest; with many, the per-call overhead of indexing dominates and
#' `split()` is faster, so `split()` is used when there are fewer than 32 rows per group on average.
#'
#' @param .data A `data.frame`.
#' @param rows A `list` of the rows of each group.
#' @return A `list` with an element for each column of `.data`, itself a `list` with the rows of that column for each
#' group.
#' @noRd
split_columns <- function(.data, rows) {
  f <- NULL
  idx <- NULL
  if (length(rows) * 32L > .row_names_info(.data, 2L)) {
    idx <- unlist(rows, use.names = FALSE)
    f <- group_factor(group_of_rows(rows), length(rows))
  }
  lapply(.data, split_column, rows = rows, idx = idx, f = f)
}

#' Split a column by group
#'
#' @param col A vector, `matrix` or `data.frame` column.
#' @param rows A `list` of the rows of each group.
#' @param idx `integer(n)`. The rows in group order, `unlist(rows)`, or `NULL` to index each group instead.
#' @param f A `factor` of the group of each element of `idx`, or `NULL` to index each group instead.
#' @return A `list` with the rows of `col` for each group.
#' @noRd
split_column <- function(col, rows, idx, f) {
  if (!is.null(dim(col))) return(lapply(rows, function(i) vec_slice(col, i)))
  cls <- oldClass(col)
  if (is.null(f)) {
    return(if (is.null(cls)) lapply(rows, function(i) .subset(col, i)) else lapply(rows, function(i) col[i]))
  }
  if (is.null(cls)) return(unname(split.default(col[idx], f)))
  attrs <- attributes(col)
  # For these classes `[` only subsets the data, so the attributes can be put back on each group
  if (inherits(col, c("factor", "Date", "POSIXct", "difftime")) &&
      all(names(attrs) %in% c("class", "levels", "tzone", "units"))) {
    return(lapply(unname(split.default(unclass(col)[idx], f)), function(x) {
      attributes(x) <- attrs
      x
    }))
  }
  lapply(rows, function(i) col[i])
}

#' Identify the group that each row belongs to
#'
#' Groups are numbered in the order in which they first appear. Missing values are treated as a value in their own
#' right.
#'
#' @param cols A `list` of equal length vectors.
#' @param n `integer(1)`. The number of rows, used when `cols` is empty.
#' @return An `integer` vector.
#' @noRd
group_ids <- function(cols, n = if (length(cols) == 0L) 0L else length(cols[[1L]])) {
  if (length(cols) == 0L) return(rep(1L, n))
  ids <- NULL
  for (col in cols) {
    if (inherits(col, "POSIXlt")) col <- as.POSIXct(col)
    if (is.factor(col) || inherits(col, c("Date", "POSIXct"))) col <- unclass(col)
    key <- first_appearance_id(col)
    ids <- if (is.null(ids)) key else first_appearance_id((ids - 1) * max(key, 0L) + key)
  }
  ids
}

#' Split row numbers by group id
#' @param ids `integer(n)`. The group ids, as returned by `group_ids()`.
#' @param n_groups `integer(1)`. The number of groups.
#' @param values `integer(n)`. The value to give for each element of `ids`; by default its position.
#' @return A `list` of `integer` vectors, one for each group, empty for a group with no `ids`.
#' @noRd
split_rows <- function(ids, n_groups, values = seq_along(ids)) {
  unname(split.default(values, group_factor(ids, n_groups)))
}

#' Find the group of each row, in group order
#' @param rows A `list` of the rows of each group.
#' @return `integer(n)`. The position in `rows` of the group of each element of `unlist(rows)`.
#' @noRd
group_of_rows <- function(rows) {
  rep.int(seq_along(rows), lengths(rows))
}

#' Make a factor of group ids, so that `split()` gives every group, even an empty one, in order
#' @param ids `integer(n)`. The group ids, each between 1 and `n_groups`.
#' @param n_groups `integer(1)`. The number of groups.
#' @return A `factor` whose codes are `ids`.
#' @noRd
group_factor <- function(ids, n_groups) {
  structure(ids, levels = as.character(seq_len(n_groups)), class = "factor")
}

# The position of each value amongst the unique values in order of first appearance, i.e. `match(x, unique(x))`.
# Whole numbers within a narrow range are indexed directly rather than hashed, which is several times faster. Below
# a few thousand values the hash table is small enough that match() is faster.
first_appearance_id <- function(x) {
  n <- length(x)
  if (n > 0L && (is.integer(x) || is.logical(x) || is.double(x)) && is.null(dim(x)) && !anyNA(x)) {
    rng <- range(x)
    span <- rng[2L] - rng[1L]
    if (span >= 4096 && span < 4 * n + 1024 && (!is.double(x) || all(x == trunc(x)))) {
      off <- as.integer(x - rng[1L]) + 1L
      first <- integer(max(off))
      first[off[n:1]] <- n:1
      present <- which(first > 0L)
      code <- integer(length(first))
      code[present[order(first[present])]] <- seq_along(present)
      return(code[off])
    }
  }
  match(x, unique(x))
}

# The position of the first appearance of each id, where `id` is as returned by `group_ids()`
first_positions <- function(id) {
  n <- length(id)
  first <- integer(max(id, 0L))
  if (n > 0L) first[id[n:1]] <- n:1
  first
}

#' Find the empty groups formed by unused factor levels
#'
#' Each factor column contributes all of its levels within each combination of the preceding
#' grouping columns. Once a group is empty, each subsequent factor is expanded to all of its levels and each subsequent
#' non-factor is set to `NA`.
#'
#' @param keys A `data.frame` of the unique observed groups.
#' @return A `data.frame` with the same columns as `keys` containing only the empty groups.
#' @noRd
expand_empty_groups <- function(keys) {
  empty <- keys[0L, , drop = FALSE]
  for (j in seq_along(keys)) {
    col <- keys[[j]]
    n_empty <- nrow(empty)
    if (!is.factor(col)) {
      empty[[j]] <- col[rep(NA_integer_, n_empty)]
      next
    }
    n_lvls <- length(levels(col))
    empty <- empty[rep(seq_len(n_empty), each = n_lvls), , drop = FALSE]
    empty[[j]] <- with_factor_codes(rep(seq_len(n_lvls), times = n_empty), col)
    prefix <- if (j == 1L) rep(1L, nrow(keys)) else group_ids(.subset(keys, seq_len(j - 1L)))
    n_prefix <- if (j == 1L) 1L else max(prefix, 0L)
    missing <- setdiff(seq_len(n_prefix * n_lvls), (prefix - 1L) * n_lvls + as.integer(col))
    if (length(missing) > 0L) {
      new <- keys[match((missing - 1L) %/% n_lvls + 1L, prefix), , drop = FALSE]
      new[[j]] <- with_factor_codes((missing - 1L) %% n_lvls + 1L, col)
      empty <- rbind(empty, new)
    }
  }
  rownames(empty) <- NULL
  empty
}

#' Check whether an object is a grouped `data.frame`
#'
#' @param x An R object.
#' @return `logical(1)`.
#' @noRd
is.grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
