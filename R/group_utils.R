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
#' other context functions. Empty groups are evaluated too, and data with no groups is evaluated once, on the empty
#' data, so that the type of each result is known.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @param size_rule The rule which the sizes of the results must follow: `group_size_rule`, where each result has the
#' size of its group or size one, as in `mutate()`, or `common_size_rule`, where the results of a group which are not
#' of size one have the same size, as in `summarise()`.
#' @param drop_null `logical(1)`. Does a `NULL` result remove the variable of that name, as in `mutate()`? If not, it is
#' ignored, as in `summarise()`.
#' @param transform A `function(value, size, group, details)` which is given each result as soon as it is evaluated and
#' returns the value to keep in its place, or `NULL` to keep each result as it is. It is given the size of the group,
#' its position `group` in `details` (`NULL` if `.data` is not grouped or has no groups) and `details`, for error
#' messages.
#' @return A `list` with:
#' * `rows`, the rows of each group that was evaluated;
#' * `details`, the grouping structure of `.data` (`NULL` if it is not grouped) and `groups`, the grouping columns;
#' * `empty`, whether `.data` has no groups and was evaluated once on its empty data;
#' * `labels`, the name of each expression, and `unnamed`, whether the name was made automatically;
#' * `values`, a `list` with an element for each expression, itself a `list` with the result for each group (`NULL`
#'   results are kept as `NULL`);
#' * `sizes`, the size of the results of each group: the size of its results which are not of size one, or one.
#' @noRd
eval_groups <- function(.data, dots, env, size_rule, drop_null = FALSE, transform = NULL) {
  layout <- group_layout(.data)
  # Grouped data with no groups is evaluated once, as a single empty group, so that the type of each result is known
  if (layout$empty) layout$rows <- list(integer())
  labels <- expression_labels(dots, env)
  # The columns that `.data` holds, in order: those of `.data`, followed by the results of the expressions
  seen <- new.env(hash = FALSE, parent = emptyenv())
  seen$col_names <- names(.data)
  masks <- group_masks(.data, layout, env, seen)

  context$setup(.data)
  on.exit(context$clean(), add = TRUE)

  values <- vector(mode = "list", length = length(dots))
  sizes <- size_rule$start(layout$rows)
  for (i in seq_along(dots)) {
    dot <- list(expr = dots[[i]], position = i, label = labels$labels[[i]], unnamed = labels$unnamed[[i]])
    vals <- eval_expression(dot, masks, layout, seen, drop_null, transform)
    sizes <- size_rule$check(sizes, vals, dot, layout)
    values[[i]] <- vals
  }

  list(
    rows = layout$rows, details = layout$details, groups = layout$groups, empty = layout$empty,
    labels = labels$labels, unnamed = labels$unnamed, values = values, sizes = sizes$sizes
  )
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

#' Name the expressions given to a verb
#'
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with `labels`, the name of each expression, and `unnamed`, whether the name was made automatically
#' by `auto_label()`.
#' @noRd
expression_labels <- function(dots, env) {
  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- character(length(dots))
  unnamed <- !nzchar(dot_names)
  labels <- dot_names
  labels[unnamed] <- vapply(dots[unnamed], auto_label, NA_character_, env = env)
  list(labels = labels, unnamed = unnamed)
}

#' Build the data mask of each group
#'
#' Most expressions never look at `.data`, so each mask holds it as an active binding which builds it from the mask
#' when it is used. As poorman has no `rowwise()`, a list column holding a single data.frame, such as the nested data of
#' a group made by `nest_by()`, is seen as that data.frame.
#'
#' @param .data A `data.frame`.
#' @param layout The groups of `.data`, as returned by `group_layout()`.
#' @param env An `environment`, the parent of each mask.
#' @param seen An `environment` whose `col_names` are the columns, in order, that `.data` holds. The evaluation adds the
#' results of the expressions to it.
#' @return A `list` with an `environment` for each group.
#' @noRd
group_masks <- function(.data, layout, env, seen) {
  chunks <- if (layout$grouped) split_columns(.data, layout$rows) else lapply(.data, list)
  mask_attrs <- mask_attributes(.data, layout)
  list_cols <- which(vapply(.data, function(col) is.list(col) && !is.data.frame(col), NA))
  lapply(seq_along(layout$rows), function(g) {
    cols <- lapply(chunks, .subset2, g)
    for (j in list_cols) {
      if (length(cols[[j]]) == 1L && is.data.frame(cols[[j]][[1L]])) cols[[j]] <- cols[[j]][[1L]]
    }
    mask <- list2env(cols, envir = new.env(hash = FALSE, parent = env))
    makeActiveBinding(".data", function() {
      res <- mget(seen$col_names, envir = mask, inherits = FALSE, ifnotfound = list(NULL))
      res <- res[!vapply(res, is.null, NA)]
      attributes(res) <- c(list(names = names(res)), mask_attrs(g))
      res
    }, mask)
    mask
  })
}

#' Build the attributes of the data each group sees as `.data`
#'
#' @param .data A `data.frame`.
#' @param layout The groups of `.data`, as returned by `group_layout()`.
#' @return A `function(g)` which gives the attributes, other than its names, of the data of group `g` as seen through
#' `.data` and the context functions.
#' @noRd
mask_attributes <- function(.data, layout) {
  if (!layout$grouped) {
    attrs <- attributes(.data)
    attrs[["names"]] <- NULL
    return(function(g) attrs)
  }
  rows <- layout$rows
  details <- layout$details
  empty <- layout$empty
  auto_row_names <- .row_names_info(.data) < 0L
  function(g) {
    list(
      class = "data.frame",
      row.names = if (auto_row_names) as.integer(rows[[g]]) else attr(.data, "row.names")[rows[[g]]],
      groups = details,
      group_id = if (empty) NA_integer_ else g
    )
  }
}

#' Evaluate one expression for each group
#'
#' Each result is stored in the mask of its group, so that the expressions which follow can use it.
#'
#' @param dot A `list` describing the expression: `expr`, the unevaluated expression, `position`, its position in the
#' `dots`, `label`, its name, and `unnamed`, whether the name was made automatically.
#' @param masks A `list` of the data mask of each group, as returned by `group_masks()`.
#' @param layout The groups, as returned by `group_layout()`.
#' @param seen An `environment` whose `col_names` are the columns that `.data` holds, updated with the result.
#' @param drop_null,transform As in `eval_groups()`.
#' @return A `list` with the result for each group.
#' @noRd
eval_expression <- function(dot, masks, layout, seen, drop_null, transform) {
  rows <- layout$rows
  details <- layout$details
  numbered <- layout$numbered
  expr <- dot$expr
  label <- dot$label
  # An unnamed data frame result, e.g. from `across()`, gives its columns rather than a column of its own
  spread <- dot$unnamed
  vals <- vector(mode = "list", length = length(masks))
  for (g in seq_along(masks)) {
    mask <- masks[[g]]
    # The data of the context functions is a promise, so that it is only built if one of them uses it. It reads the
    # `.data` of `mask` when it is forced, which can only happen while this group is evaluated
    delayedAssign(".data", get(".data", envir = mask, inherits = FALSE), eval.env = environment(), assign.env = context)
    value <- eval(expr, mask)
    if (!is.null(transform)) value <- transform(value, length(rows[[g]]), if (numbered) g, details)
    if (is.null(value)) {
      if (drop_null && exists(label, envir = mask, inherits = FALSE)) rm(list = label, envir = mask)
      next
    }
    vals[g] <- list(value)
    if (spread && is.data.frame(value)) {
      list2env(value, envir = mask)
      seen$col_names <- union(seen$col_names, names(value))
    } else {
      assign(label, value, envir = mask)
    }
  }
  track_result(seen, vals, dot, drop_null)
  vals
}

#' Add the result of an expression to the columns that `.data` holds
#'
#' @param seen An `environment` whose `col_names` are the columns that `.data` holds.
#' @param vals A `list` with the result of the expression for each group.
#' @param dot A `list` describing the expression, as given to `eval_expression()`.
#' @param drop_null `logical(1)`. Does a `NULL` result remove the variable of that name?
#' @return `NULL`, invisibly; `seen` is updated in place.
#' @noRd
track_result <- function(seen, vals, dot, drop_null) {
  if (drop_null && all(vapply(vals, is.null, NA))) {
    seen$col_names <- setdiff(seen$col_names, dot$label)
  } else if (!dot$unnamed || !all(vapply(vals, is.data.frame, NA))) {
    seen$col_names <- union(seen$col_names, dot$label)
  }
  invisible()
}

#' Rules for the sizes of the results of each group
#'
#' A size rule is a `list` of two functions, used by `eval_groups()`:
#' * `start(rows)` gives the sizes before any expression is evaluated, as a `list` with `sizes`, the size of each
#'   group's results;
#' * `check(sizes, vals, dot, layout)` checks the results `vals` of an expression for each group against `sizes`, gives
#'   an error for a result of the wrong size and returns the sizes for the expressions which follow. `dot` describes the
#'   expression, as given to `eval_expression()`, and `layout` gives the groups, as returned by `group_layout()`.
#'
#' With `group_size_rule`, as in `mutate()`, each result must have the size of its group or size one. With
#' `common_size_rule`, as in `summarise()`, the results of a group which are not of size one must have the same size,
#' which is set by the first of them; `start()` also gives `from`, the name of the expression which set the size of
#' each group, or `""`.
#' @noRd
group_size_rule <- list(
  start = function(rows) list(sizes = lengths(rows)),
  check = function(sizes, vals, dot, layout) {
    vals_size <- vapply(vals, NROW, 1L)
    wrong <- which(!vapply(vals, is.null, NA) & vals_size != 1L & vals_size != sizes$sizes)
    if (length(wrong) > 0L) {
      g <- wrong[[1L]]
      stop_group_size(dot, sizes$sizes[[g]], vals_size[[g]], if (layout$numbered) g, layout$details)
    }
    sizes
  }
)

#' The size rule of `summarise()`
#'
#' See `group_size_rule`.
#' @noRd
common_size_rule <- list(
  start = function(rows) list(sizes = rep(1L, length(rows)), from = character(length(rows))),
  check = function(sizes, vals, dot, layout) {
    vals_size <- vapply(vals, NROW, 1L)
    resized <- !vapply(vals, is.null, NA) & vals_size != 1L & vals_size != sizes$sizes
    wrong <- which(resized & sizes$sizes != 1L)
    if (length(wrong) > 0L) {
      g <- wrong[[1L]]
      stop_common_size(dot$label, sizes$sizes[[g]], vals_size[[g]], sizes$from[[g]], if (layout$numbered) g)
    }
    sizes$sizes[resized] <- vals_size[resized]
    sizes$from[resized] <- dot$label
    sizes
  }
)

#' Give the error for a result which does not have the size of its group or size one
#'
#' @param dot A `list` describing the expression, as given to `eval_expression()`.
#' @param size `integer(1)`. The size of the group.
#' @param value_size `integer(1)`. The size of the result.
#' @param group `integer(1)`. The group of the result, or `NULL` if the data is not grouped.
#' @param details A `data.frame` of the groups, as returned by `get_groups()`.
#' @return Does not return; gives an error.
#' @noRd
stop_group_size <- function(dot, size, value_size, group, details) {
  # As in dplyr, the error of `mutate()` calls an unnamed expression by its position
  name <- if (dot$unnamed) paste0("..", dot$position) else dot$label
  stop(
    sprintf("Problem while computing `%s = %s`.\n", name, paste(deparse(dot$expr), collapse = " ")),
    sprintf("x `%s` must be size %d or 1, not %d.", name, size, value_size),
    error_group(group, details),
    call. = FALSE
  )
}

#' Give the error for a result whose size differs from the earlier results of its group
#'
#' @param name `character(1)`. The name of the expression.
#' @param size `integer(1)`. The size the result should have.
#' @param value_size `integer(1)`. The size of the result.
#' @param size_from `character(1)`. The name of the earlier expression which set `size`, or `""` if none did.
#' @param group `integer(1)`. The group of the result, or `NULL` if the data is not grouped.
#' @return Does not return; gives an error.
#' @noRd
stop_common_size <- function(name, size, value_size, size_from, group) {
  stop(
    sprintf("`%s` must be size %d or 1, not %d.", name, size, value_size),
    if (nzchar(size_from)) sprintf(" An earlier column, `%s`, had size %d.", size_from, size),
    if (!is.null(group)) sprintf(" The error occurred in group %d.", group),
    call. = FALSE
  )
}

#' Give the line of an error message which names the group where the error occurred
#'
#' @param group `integer(1)`. The position of the group in `details`, or `NULL` if the data is not grouped.
#' @param details A `data.frame` of the groups, as returned by `get_groups()`.
#' @return `character(1)`, starting with a new line, or `NULL` if `group` is `NULL`.
#' @noRd
error_group <- function(group, details) {
  if (!is.null(group)) sprintf("\ni The error occurred in group %d: %s.", group, describe_group(details, group))
}

#' Describe a group by its keys
#'
#' As in the error messages of dplyr, each key is given as `name = value`, with character values in quotes, e.g.
#' `g = "a", h = 1`.
#'
#' @param details A `data.frame` of the groups, as returned by `get_groups()`.
#' @param g `integer(1)`. The position of the group in `details`.
#' @return `character(1)`.
#' @noRd
describe_group <- function(details, g) {
  keys <- setdiff(names(details), ".rows")
  values <- vapply(keys, function(key) {
    value <- .subset2(details, key)[g]
    if (is.character(value)) encodeString(value, quote = "\"") else as.character(value)
  }, NA_character_)
  paste0(keys, " = ", values, collapse = ", ")
}

#' Combine the results of an expression across the groups
#'
#' Results of size one are recycled to the size of their group. As in dplyr, `NULL` results are ignored, but `NULL`
#' results cannot be combined with other results.
#'
#' @param vals A `list` with the result of the expression for each group, as returned by `eval_groups()`.
#' @param sizes `integer(n)`. The size of each group's results, as returned by `eval_groups()`.
#' @param label `character(1)`. The name of the expression, for error messages.
#' @return The combined results, in group order, or `NULL` if every result was `NULL`.
#' @noRd
combine_groups <- function(vals, sizes, label) {
  is_null <- vapply(vals, is.null, NA)
  if (all(is_null)) return(NULL)
  if (any(is_null)) {
    stop(
      sprintf("`%s` must return compatible vectors across groups: ", label),
      "it can't combine NULL and non NULL results.",
      call. = FALSE
    )
  }
  recycle <- which(vapply(vals, NROW, 1L) != sizes)
  vals[recycle] <- lapply(recycle, function(g) vec_slice(vals[[g]], rep(1L, sizes[[g]])))
  if (length(vals) == 1L) vals[[1L]] else combine_values(vals)
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

#' Combine the values of each group into one vector
#'
#' Values without attributes are combined directly; factors, plain `Date`s and unclassed lists use `combine_columns()`,
#' so they follow the type rules of `pivot_longer()` and incompatible types are an error; data frames and matrices are
#' bound by row; anything else falls back to `rbind()`.
#'
#' @param x A `list` of values, one for each group, none of them `NULL`.
#' @return The combined values.
#' @noRd
combine_values <- function(x) {
  if (all(lengths(lapply(x, attributes)) == 0L)) {
    is_atomic <- vapply(x, is.atomic, NA)
    if (all(is_atomic) || !any(is_atomic) && all(vapply(x, is.list, NA))) {
      return(unlist(x, recursive = FALSE, use.names = FALSE))
    }
  }
  if (all(vapply(x, is_simple_column, NA))) return(combine_columns(x))
  if (all(vapply(x, is.data.frame, NA))) return(do.call(rbind, unname(x)))
  if (all(vapply(x, is.matrix, NA))) return(do.call(rbind, unname(x)))
  x <- lapply(x, function(v) structure(list(v), names = "x", class = "data.frame", row.names = .set_row_names(NROW(v))))
  .subset2(do.call(rbind, x), 1L)
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
    f <- structure(rep.int(seq_along(rows), lengths(rows)), levels = as.character(seq_along(rows)), class = "factor")
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

#' Make a factor from its codes
#'
#' @param codes `integer(n)`. The codes, i.e. positions in the levels of `template`.
#' @param template A `factor` whose levels and class are used.
#' @return A `factor` with the levels and class of `template`.
#' @noRd
with_factor_codes <- function(codes, template) {
  structure(as.integer(codes), levels = levels(template), class = class(template))
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

#' Check whether a column can be combined by `combine_columns()` in `combine_values()`
#'
#' @param col A column of a `data.frame`.
#' @return `logical(1)`. `TRUE` for a factor, a plain `Date`, or an atomic vector or unclassed list with no attributes
#' other than names.
#' @noRd
is_simple_column <- function(col) {
  if (is.factor(col)) return(TRUE)
  attrs <- names(attributes(col))
  (is.atomic(col) || (is.list(col) && is.null(attr(col, "class")))) && all(attrs %in% "names") ||
    inherits(col, "Date") && all(attrs %in% "class") && identical(class(col), "Date")
}

#' Check whether an object is a grouped `data.frame`
#'
#' @param x An R object.
#' @return `logical(1)`.
#' @noRd
is.grouped_df <- function(x) {
  inherits(x, "grouped_df")
}
