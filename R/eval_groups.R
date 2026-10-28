#' Evaluate expressions for each group
#'
#' This is the engine of the grouped verbs. Each column of `.data` is split by group once, rather than splitting
#' `.data` into a `data.frame` per group. Each group gets its own data mask, an environment holding the columns of the
#' group whose parent is `env`, the caller's environment, so that local variables are found and the variables of this
#' function never are. Each expression is evaluated for every group before the next expression, and each
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
#' * `numbered`, whether `.data` has groups, so that a group can be named by its position in `details`;
#' * `exprs`, the expressions, `labels`, the name of each, and `unnamed`, whether the name was made automatically;
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
  # The columns made by the expressions, and the size of the results of each group so far
  seen$results <- character()
  masks <- group_masks(.data, layout, env, seen)

  context$setup(.data)
  on.exit(context$clean(), add = TRUE)

  values <- vector(mode = "list", length = length(dots))
  sizes <- size_rule$start(layout$rows)
  seen$sizes <- sizes$sizes
  for (i in seq_along(dots)) {
    # The columns made by earlier expressions are fixed while this one is evaluated, so the reader keeps those it builds
    context$full_data <- full_data_reader(.data, masks, layout, seen, size_rule$aligned)
    dot <- list(expr = dots[[i]], position = i, label = labels$labels[[i]], unnamed = labels$unnamed[[i]])
    vals <- eval_expression(dot, masks, layout, seen, drop_null, transform)
    sizes <- size_rule$check(sizes, vals, dot, layout)
    seen$sizes <- sizes$sizes
    values[[i]] <- vals
  }

  list(
    rows = layout$rows, details = layout$details, groups = layout$groups, empty = layout$empty,
    numbered = layout$numbered, exprs = dots, labels = labels$labels, unnamed = labels$unnamed, values = values,
    sizes = sizes$sizes
  )
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
  # Each `across()` call of the expression selects its columns in the first group it is evaluated in and keeps them for
  # the other groups
  context$across_setups <- list()
  for (g in seq_along(masks)) {
    mask <- masks[[g]]
    context$mask <- mask
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
  # The columns of a data frame result are spread into the masks, so `.data` holds them. They are added once all of
  # the groups are evaluated: until then, a group only holds the columns of its own result
  is_df <- vapply(vals, is.data.frame, NA)
  if (dot$unnamed && any(is_df)) {
    seen$col_names <- union(seen$col_names, unlist(lapply(vals[is_df], names), use.names = FALSE))
  }
  if (drop_null && all(vapply(vals, is.null, NA))) {
    seen$col_names <- setdiff(seen$col_names, dot$label)
  } else if (!dot$unnamed || !all(is_df)) {
    seen$col_names <- union(seen$col_names, dot$label)
    seen$results <- union(seen$results, dot$label)
  } else {
    seen$results <- union(seen$results, unlist(lapply(vals, names), use.names = FALSE))
  }
  invisible()
}

#' Build the full columns of the data that the expressions see
#'
#' `where()` in `across()` applies its predicate to each full column, rather than to the rows of one group.
#' The columns of `.data` are already whole; a column made by an earlier expression is only built, by combining the
#' results of the groups, when it is asked for.
#'
#' @param .data,layout As in `eval_expression()`.
#' @param masks A `list` of the data mask of each group, as returned by `group_masks()`.
#' @param seen An `environment` with `col_names`, the columns that the expressions see, `results`, the columns made by
#' the expressions, and `sizes`, the size of the results of each group so far.
#' @param aligned `logical(1)`. Do the results of a group line up with its rows, as in `mutate()`? If so, a column made
#' by an expression is put in the order of the rows of `.data`; otherwise its results are in the order of the groups.
#' @return A `function(names)` which gives a `list` of the full columns called `names`.
#' @noRd
full_data_reader <- function(.data, masks, layout, seen, aligned) {
  # A call to `across()` made from a function selects its columns in every group, so a column made by an earlier
  # expression is only combined once
  built <- new.env(hash = TRUE, parent = emptyenv())
  # The order of the rows is found when the first column is built, as most expressions build none
  order <- NULL
  ordered <- FALSE
  build <- function(name) {
    vals <- lapply(masks, function(mask) get0(name, envir = mask, inherits = FALSE))
    sizes <- seen$sizes
    recycle <- which(vapply(vals, NROW, 1L) == 1L & sizes != 1L)
    vals[recycle] <- lapply(recycle, function(g) vec_slice(vals[[g]], rep(1L, sizes[[g]])))
    col <- if (length(vals) == 1L) vals[[1L]] else combine_values(vals)
    if (!aligned) return(col)
    if (!ordered) {
      order <<- ungroup_order(layout$rows, .row_names_info(.data, 2L))
      ordered <<- TRUE
    }
    if (is.null(order)) col else vec_slice(col, order)
  }
  function(names) {
    cols <- lapply(names, function(name) {
      if (!(name %in% seen$results)) return(.subset2(.data, name))
      if (!exists(name, envir = built, inherits = FALSE)) assign(name, build(name), envir = built)
      get(name, envir = built, inherits = FALSE)
    })
    names(cols) <- names
    cols
  }
}

#' Rules for the sizes of the results of each group
#'
#' A size rule is a `list` of `aligned`, whether the results of each group line up with its rows, and two functions,
#' used by `eval_groups()`:
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
  aligned = TRUE,
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
  aligned = FALSE,
  start = function(rows) list(sizes = rep(1L, length(rows)), from = character(length(rows))),
  check = function(sizes, vals, dot, layout) {
    vals_size <- vapply(vals, NROW, 1L)
    unsized <- vapply(vals, function(v) is.null(v) || (dot$unnamed && is.data.frame(v) && length(v) == 0L), NA)
    resized <- !unsized & vals_size != 1L & vals_size != sizes$sizes
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
  name <- if (dot$unnamed) paste0("..", dot$position) else dot$label
  stop(
    computing_problem(dot),
    # When the group has one row, the size is given once
    sprintf("x `%s` must be size %s, not %d.", name, if (size == 1L) "1" else paste(size, "or 1"), value_size),
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

#' Give the first line of an error in an expression
#'
#' The line calls an unnamed expression by its position, e.g. "Problem while computing `..1 = 1:2`.".
#'
#' @param dot A `list` describing the expression, as given to `eval_expression()`.
#' @return `character(1)`, ending with a new line.
#' @noRd
computing_problem <- function(dot) {
  name <- if (dot$unnamed) paste0("..", dot$position) else dot$label
  sprintf("Problem while computing `%s = %s`.\n", name, paste(deparse(dot$expr), collapse = " "))
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
#' Each key is given as `name = value`, with character values in quotes, e.g.
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
#' Results of size one are recycled to the size of their group. `NULL` results are ignored, but `NULL`
#' results cannot be combined with other results, and results of incompatible types are an error.
#'
#' @param res The result of `eval_groups()`.
#' @param i `integer(1)`. The position of the expression whose results are combined.
#' @param sizes `integer(n)`. The size of each group's results.
#' @return The combined results, in group order, or `NULL` if every result was `NULL`.
#' @noRd
combine_groups <- function(res, i, sizes = res$sizes) {
  vals <- res$values[[i]]
  is_null <- vapply(vals, is.null, NA)
  if (all(is_null)) return(NULL)
  dot <- list(expr = res$exprs[[i]], position = i, label = res$labels[[i]], unnamed = res$unnamed[[i]])
  if (any(is_null)) {
    name <- if (dot$unnamed) paste0("..", i) else dot$label
    stop(
      computing_problem(dot),
      sprintf("x `%s` must return compatible vectors across groups.\n", name),
      "x Can't combine NULL and non NULL results.",
      call. = FALSE
    )
  }
  if (length(vals) > 1L) check_group_types(vals, dot, res$details)
  recycle <- which(vapply(vals, NROW, 1L) != sizes)
  vals[recycle] <- lapply(recycle, function(g) vec_slice(vals[[g]], rep(1L, sizes[[g]])))
  if (length(vals) == 1L) vals[[1L]] else combine_values(vals)
}

#' Check that the results of an expression can be combined across the groups
#'
#' Results are compatible when they are of the same kind, as given by `result_kind()`. Within a kind, the existing rules
#' of `combine_values()` apply, e.g. integers and doubles combine to a double. A logical vector of only `NA` is
#' compatible with any kind. A complex result is a number, but the results so far cannot be complex when
#' the next is logical, nor logical when the next is complex.
#'
#' Data frames are compatible when each of their columns is, so the columns of `across()` follow the same rules as any
#' other result.
#'
#' @param vals A `list` with the result of the expression for each group, none of them `NULL`.
#' @param dot A `list` describing the expression, as given to `eval_expression()`.
#' @param details A `data.frame` of the groups, as returned by `get_groups()`.
#' @param name `character(1)`. The name of the results in the error message.
#' @return `NULL`, invisibly, or gives an error.
#' @noRd
check_group_types <- function(vals, dot, details, name = dot$label) {
  # Most results are plain vectors of one type, or of numbers, which can always be combined; this is checked first as
  # it is much faster than finding the kind of each result
  if (is_one_type(plain_types(vals))) return(invisible())
  kinds <- vapply(vals, result_kind, NA_character_)
  # Whether a complex result can be combined depends on the types before it, so the groups are followed in order
  if (length(unique(kinds[nzchar(kinds)])) > 1L || "complex" %in% vapply(vals, typeof, NA_character_)) {
    clash <- find_type_clash(vals, kinds)
    if (!is.null(clash)) stop_group_types(clash, dot, details, name)
  }
  if (all(kinds == "data.frame")) check_frame_types(vals, dot, details)
  invisible()
}

#' Check that the columns of data frame results can be combined across the groups, for `check_group_types()`
#'
#' The error for an unnamed result, e.g. from `across()`, names the column; a named result keeps its name.
#' Data frames with different columns are left to `combine_values()`.
#'
#' @param vals A `list` with the data frame result of the expression for each group.
#' @param dot,details As in `check_group_types()`.
#' @return `NULL`, invisibly, or gives an error.
#' @noRd
check_frame_types <- function(vals, dot, details) {
  nms <- names(vals[[1L]])
  if (!all(vapply(vals, function(df) identical(names(df), nms), NA))) return(invisible())
  for (j in seq_along(nms)) {
    check_group_types(lapply(vals, .subset2, j), dot, details, name = if (dot$unnamed) nms[[j]] else dot$label)
  }
  invisible()
}

#' Give the error for results of incompatible types
#'
#' The error names two groups: the group whose result gave the type of the results before the first
#' incompatible one, and the group of that incompatible result.
#'
#' @param clash The incompatible results, as returned by `find_type_clash()`.
#' @param dot A `list` describing the expression, as given to `eval_expression()`.
#' @param details A `data.frame` of the groups, as returned by `get_groups()`.
#' @param name `character(1)`. The name of the results, as in `check_group_types()`.
#' @return Does not return; gives an error.
#' @noRd
stop_group_types <- function(clash, dot, details, name = dot$label) {
  line <- "\ni Result type for group %d (%s): <%s>."
  stop(
    computing_problem(dot),
    "Caused by error:\n",
    sprintf("! `%s` must return compatible vectors across groups.", name),
    sprintf(line, clash$ref, describe_group(details, clash$ref), clash$ref_type),
    sprintf(line, clash$g, describe_group(details, clash$g), clash$type),
    call. = FALSE
  )
}
