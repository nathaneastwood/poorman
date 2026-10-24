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

#' Apply a function to each group
#'
#' The function is given as a closure of one argument so that the names of the user's expressions, e.g.
#' `mutate(x, .fn = 1)`, never meet the arguments of this function.
#' @param .data A `data.frame`.
#' @param .fn A function of one argument, a `data.frame`, applied to each group, e.g. `function(df) mutate(df, ...)`.
#' @param .drop `logical(1)`. Should empty groups be skipped?
#' @param .reorder `logical(1)`. Should the rows of the result be put back into the order they appear in `.data`? This
#' relies on `.fn` returning rows with the row names of `.data`.
#' @noRd
apply_grouped_function <- function(.data, .fn, .drop = FALSE, .reorder = FALSE) {
  groups <- group_vars(.data)
  grouped <- split_into_groups(.data, groups, .drop)
  res <- bind_groups(lapply(grouped, .fn))
  if (is.null(res)) return(res)
  attr(res, "group_id") <- NULL
  if (isTRUE(.reorder)) {
    pos <- match(rownames(res), rownames(.data))
    if (!anyNA(pos)) {
      pos <- sort(pos)
      res <- subset_rows(res, order(match(rownames(res), rownames(.data))))
      attr(res, "row.names") <- attr(.data, "row.names")[pos]
    }
  }
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

is_valid_groups <- function(.data, details, groups) {
  if (!is.data.frame(details) || !identical(setdiff(names(details), ".rows"), groups)) return(FALSE)
  rows <- details[[".rows"]]
  idx <- unlist(rows, use.names = FALSE)
  n <- nrow(.data)
  # Every row must appear exactly once; tabulate() ignores missing and out of range values so they fail this too
  if (!is.numeric(idx) || length(idx) != n || any(tabulate(idx, n) != 1L)) return(FALSE)
  group <- rep.int(seq_along(rows), lengths(rows))
  for (col in groups) {
    if (!identical(unname(.subset2(.data, col)[idx]), unname(.subset2(details, col)[group]))) return(FALSE)
  }
  TRUE
}

#' Evaluate expressions for each group
#'
#' This is the engine of the grouped verbs. Each column of `.data` is split by group once, rather than splitting
#' `.data` into a `data.frame` per group. Each group gets its own data mask, an environment holding the columns of the
#' group whose parent is `env`, the caller's environment, so that local variables are found and the variables of this
#' function never are. As in dplyr, each expression is evaluated for every group before the next expression, and each
#' result is available to the expressions that follow it, both as a variable and through `.data`, `across()` and the
#' other context functions. Data with no groups is evaluated once, on the empty data, so that the type of each result
#' is known.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with:
#' * `rows`, the rows of each group that was evaluated;
#' * `details`, the grouping structure of `.data` (`NULL` if it is not grouped) and `groups`, the grouping columns;
#' * `empty`, whether `.data` has no groups and was evaluated once on its empty data;
#' * `labels`, the name of each expression, and `unnamed`, whether the name was made automatically;
#' * `values`, a `list` with an element for each expression, itself a `list` with the result for each group (`NULL`
#'   results are kept as `NULL`);
#' * `sizes`, the size of the results of each group: the size of its results which are not of size one, or one.
#' @noRd
eval_groups <- function(.data, dots, env) {
  groups <- group_vars(.data)
  grouped <- length(groups) > 0L
  details <- if (grouped) get_groups(.data, groups)
  rows <- if (grouped) details[[".rows"]] else list(seq_len(.row_names_info(.data, 2L)))
  empty <- length(rows) == 0L
  if (empty) rows <- list(integer())
  n_groups <- length(rows)

  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- character(length(dots))
  unnamed <- !nzchar(dot_names)
  labels <- dot_names
  labels[unnamed] <- vapply(dots[unnamed], auto_label, NA_character_, env = env)

  chunks <- if (grouped) split_columns(.data, rows) else lapply(.data, list)
  # The columns of `.data`, in order, followed by the results of the expressions
  col_names <- names(.data)
  # Other than its names, the attributes of the data each group sees as `.data` and through the context functions
  slice_attrs <- if (grouped) {
    auto_row_names <- .row_names_info(.data) < 0L
    function(g) {
      list(
        class = "data.frame",
        row.names = if (auto_row_names) as.integer(rows[[g]]) else attr(.data, "row.names")[rows[[g]]],
        groups = details,
        group_id = if (empty) NA_integer_ else g
      )
    }
  } else {
    attrs <- attributes(.data)
    attrs[["names"]] <- NULL
    function(g) attrs
  }
  # Most expressions never look at `.data`, so it is an active binding which builds it from the mask when it is used
  masks <- lapply(seq_len(n_groups), function(g) {
    mask <- list2env(lapply(chunks, .subset2, g), envir = new.env(hash = FALSE, parent = env))
    makeActiveBinding(".data", function() {
      res <- mget(col_names, envir = mask, inherits = FALSE, ifnotfound = list(NULL))
      res <- res[!vapply(res, is.null, NA)]
      attributes(res) <- c(list(names = names(res)), slice_attrs(g))
      res
    }, mask)
    mask
  })

  # Restore any outer context so that a verb inside another verb does not break it
  outer <- get0(".data", envir = context, inherits = FALSE)
  on.exit(add = TRUE, {
    if (!is.null(outer)) {
      context$.data <- outer
    } else if (exists(".data", envir = context, inherits = FALSE)) {
      context$clean()
    }
  })

  values <- vector(mode = "list", length = length(dots))
  sizes <- rep(1L, n_groups)
  size_from <- character(n_groups)
  for (i in seq_along(dots)) {
    vals <- vector(mode = "list", length = n_groups)
    for (g in seq_len(n_groups)) {
      mask <- masks[[g]]
      delayedAssign(".data", .data, eval.env = mask, assign.env = context)
      value <- eval(dots[[i]], mask)
      if (is.null(value)) next
      vals[g] <- list(value)
      if (unnamed[[i]] && is.data.frame(value)) {
        list2env(value, envir = mask)
        col_names <- union(col_names, names(value))
      } else {
        assign(labels[[i]], value, envir = mask)
      }
    }
    if (!unnamed[[i]] || !all(vapply(vals, is.data.frame, NA))) col_names <- union(col_names, labels[[i]])
    vals_size <- vapply(vals, NROW, 1L)
    resized <- !vapply(vals, is.null, NA) & vals_size != 1L & vals_size != sizes
    wrong <- which(resized & sizes != 1L)
    if (length(wrong) > 0L) {
      g <- wrong[[1L]]
      stop(
        sprintf("`%s` must be size %d or 1, not %d.", labels[[i]], sizes[[g]], vals_size[[g]]),
        if (nzchar(size_from[[g]])) sprintf(" An earlier column, `%s`, had size %d.", size_from[[g]], sizes[[g]]),
        if (grouped && !empty) sprintf(" The error occurred in group %d.", g),
        call. = FALSE
      )
    }
    sizes[resized] <- vals_size[resized]
    size_from[resized] <- labels[[i]]
    values[[i]] <- vals
  }

  list(
    rows = rows, details = details, groups = groups, empty = empty, labels = labels, unnamed = unnamed,
    values = values, sizes = sizes
  )
}

# Split each column of `.data` by the row numbers in `rows`. The result is a list with an element for each column,
# itself a list with an element for each group. With few groups, indexing each group is fastest; with many, the
# per-call overhead of indexing dominates and `split()` is faster.
split_columns <- function(.data, rows) {
  f <- NULL
  idx <- NULL
  if (length(rows) * 32L > .row_names_info(.data, 2L)) {
    idx <- unlist(rows, use.names = FALSE)
    f <- structure(rep.int(seq_along(rows), lengths(rows)), levels = as.character(seq_along(rows)), class = "factor")
  }
  lapply(.data, split_column, rows = rows, idx = idx, f = f)
}

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
#' @return A `list` of `integer` vectors.
#' @noRd
split_rows <- function(ids, n_groups) {
  f <- structure(ids, levels = as.character(seq_len(n_groups)), class = "factor")
  unname(split.default(seq_along(ids), f))
}

#' Find the empty groups formed by unused factor levels
#'
#' Following `dplyr`, each factor column contributes all of its levels within each combination of the preceding
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

with_factor_codes <- function(codes, template) {
  structure(as.integer(codes), levels = levels(template), class = class(template))
}

#' Split a `data.frame` into groups.
#'
#' Each element of the result carries the full grouping structure in its `"groups"` attribute and the position of the
#' group within that structure in its `"group_id"` attribute.
#'
#' @param .data A `data.frame`.
#' @param groups `character(n)`. A `vector` of grouping variables to split by.
#' @param drop `logical(1)`. Should empty groups be skipped?
#'
#' @return A `list` with a `data.frame` for each group.
#' @noRd
split_into_groups <- function(.data, groups, drop = FALSE) {
  details <- get_groups(.data, groups)
  rows <- details[[".rows"]]
  ids <- if (isTRUE(drop)) which(lengths(rows) > 0L) else seq_along(rows)
  class(.data) <- "data.frame"
  attr(.data, "groups") <- details
  if (length(ids) == 0L) {
    return(list(structure(subset_rows(.data, integer()), group_id = NA_integer_)))
  }
  lapply(ids, function(i) structure(subset_rows(.data, rows[[i]]), group_id = i))
}

#' Select rows of a `data.frame`
#'
#' A faster equivalent of `x[i, , drop = FALSE]` for a `data.frame` whose columns are all vectors. Other attributes of
#' `x` are kept.
#'
#' @param x A `data.frame`.
#' @param i `integer(n)`. The rows to select.
#' @noRd
subset_rows <- function(x, i) {
  if (!all(vapply(x, function(col) is.null(dim(col)), NA))) return(x[i, , drop = FALSE])
  attrs <- attributes(x)
  res <- lapply(x, `[`, i)
  # The stored row names can be the compact form c(NA, n), so the expanded ones from `attr()` are indexed
  attrs[["row.names"]] <- if (is.integer(.row_names_info(x, type = 0L)) && .row_names_info(x) < 0L) {
    as.integer(i)
  } else {
    attr(x, "row.names")[i]
  }
  attributes(res) <- attrs
  res
}

#' Combine the results of applying a function to each group
#'
#' A faster equivalent of `do.call(rbind, x)` for the common case where each element is a `data.frame` with the same
#' column names and simple vector columns. Anything else falls back to `rbind()`.
#'
#' @param x A `list` of `data.frame`s.
#' @noRd
bind_groups <- function(x) {
  x <- unname(x)
  if (length(x) <= 1L) return(if (length(x) == 0L) NULL else x[[1L]])
  first <- x[[1L]]
  nms <- names(first)
  simple <- all(vapply(x, function(df) is.data.frame(df) && identical(names(df), nms), NA)) &&
    !anyDuplicated(nms) &&
    all(vapply(x, function(df) all(vapply(df, is_simple_column, NA)), NA))
  if (!simple) return(do.call(rbind, x))
  cols <- lapply(seq_along(nms), function(j) combine_columns(lapply(x, .subset2, j)))
  if (any(vapply(cols, is.null, NA))) return(do.call(rbind, x))
  attrs <- attributes(first)
  attrs[["row.names"]] <- bind_row_names(x)
  attributes(cols) <- attrs
  cols
}

is_simple_column <- function(col) {
  if (is.factor(col)) return(TRUE)
  attrs <- names(attributes(col))
  (is.atomic(col) || (is.list(col) && is.null(attr(col, "class")))) && all(attrs %in% "names") ||
    inherits(col, "Date") && all(attrs %in% "class") && identical(class(col), "Date")
}

combine_columns <- function(cols) {
  is_fct <- vapply(cols, is.factor, NA)
  if (all(is_fct)) {
    lvls <- unique(unlist(lapply(cols, levels), use.names = FALSE))
    codes <- unlist(lapply(cols, function(col) match(levels(col), lvls)[col]), use.names = FALSE)
    cls <- class(cols[[1L]])
    if (!all(vapply(cols, function(col) identical(class(col), cls) && identical(levels(col), lvls), NA))) {
      cls <- "factor"
    }
    return(structure(if (is.null(codes)) integer() else codes, levels = lvls, class = cls))
  }
  if (any(is_fct)) return(NULL)
  is_date <- vapply(cols, inherits, NA, "Date")
  if (all(is_date)) return(structure(unlist(lapply(cols, unclass), use.names = FALSE), class = "Date"))
  if (any(is_date)) return(NULL)
  is_list <- vapply(cols, is.list, NA)
  if (any(is_list) && !all(is_list)) return(NULL)
  unlist(cols, recursive = FALSE, use.names = FALSE)
}

# Follows the row name rules of `rbind.data.frame()`.
bind_row_names <- function(x) {
  auto <- TRUE
  n <- 0L
  labels <- vector(mode = "list", length = length(x))
  for (k in seq_along(x)) {
    rn <- attr(x[[k]], "row.names")
    n_k <- length(rn)
    if (auto && n > 0L && identical(rn, seq_len(n_k))) {
      rn <- seq.int(from = n + 1L, length.out = n_k)
    } else if (auto && (n > 0L || !identical(rn, seq_len(n_k)))) {
      auto <- FALSE
    }
    labels[[k]] <- rn
    n <- n + n_k
  }
  if (auto) return(.set_row_names(n))
  labels <- unlist(labels, use.names = FALSE)
  if (anyDuplicated(labels)) labels <- make.unique(as.character(labels), sep = "")
  labels
}

is.grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
