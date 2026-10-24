#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarising the variables of an existing `data.frame`. Grouped `data.frame`s will
#' result in one row in the output for each group.
#'
#' `summarise()` and `summarize()` are synonyms.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of summary functions. The name will be the name of the variable in the result.
#'
#' A name can also be given with `:=`, e.g. `total := sum(x)`. A string on the left of `:=` is a `gluestick` template
#' which is filled in from the calling environment, so that a name can be built from a variable, e.g.
#' `"{var}_mean" := mean(x)`.
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
#' @section Automatic names:
#' An unnamed expression is named after its code, e.g. `mean(disp)`, and `.data$disp` is named `disp`. When the code
#' is too long to read as a name, it is abbreviated with `...`. A call keeps its function, e.g. `mean(...)`, and an
#' operator keeps as many of its two sides as fit in 60 characters, e.g. `... %>% select(a, b)`.
#'
#' The names are those of `dplyr`, with these differences:
#' * The limit of 60 characters is fixed. `dplyr` keeps some names of up to 63 characters whose right side is a call,
#'   such as a long name followed by `+ g()`.
#' * Characters are counted rather than bytes, so names with accented or other multibyte characters are not
#'   abbreviated sooner.
#' * `?` and `:=` are treated as calls rather than operators, e.g. `` `?`(...) ``, as R prints them.
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
  summarise_groups(.data, dotdotdot(...), parent.frame())$data
}

#' @export
summarise.grouped_df <- function(.data, ..., .groups = NULL) {
  if (!is.null(.groups)) {
    .groups <- match.arg(arg = .groups, choices = c("drop", "drop_last", "keep"), several.ok = FALSE)
  }

  groups <- group_vars(.data)
  res <- summarise_groups(.data, dotdotdot(...), parent.frame())

  verbose <- summarise_verbose(.groups)

  # As in dplyr, the grouping is kept when any group gave other than one row
  if (is.null(.groups)) .groups <- if (res$all_one) "drop_last" else "keep"
  res <- res$data

  if (.groups == "drop_last") {
    n <- length(groups)
    if (n > 1) {
      if (verbose) summarise_inform(groups[-n])
      res <- summarise_regroup(res, groups[-n], group_by_drop_default(.data))
    }
  } else if (.groups == "keep") {
    if (verbose) summarise_inform(groups)
    res <- summarise_regroup(res, groups, group_by_drop_default(.data))
  }

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

#' Evaluate summary expressions for each group
#'
#' Each column of `.data` is split by group once, rather than splitting `.data` into a `data.frame` per group. The
#' expressions are evaluated for each group in a data mask whose parent is `env`, the caller's environment, so that
#' local variables are found and the variables of this function never are. As in dplyr, each result is available to
#' the expressions that follow it, results of size one are recycled to the size of the group and the results of a
#' column are combined across the groups at the end.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with `data`, the result as a `data.frame` (the grouping columns followed by the summaries), and
#' `all_one`, whether every group gave exactly one row.
#' @noRd
summarise_groups <- function(.data, dots, env) {
  groups <- group_vars(.data)
  grouped <- length(groups) > 0L
  n_rows <- .row_names_info(.data, 2L)
  details <- if (grouped) get_groups(.data, groups)
  rows <- if (grouped) details[[".rows"]] else list(seq_len(n_rows))
  n_groups <- length(rows)

  if (length(dots) == 0L) {
    return(summarise_result(if (grouped) as.list(details)[groups], list(), rep(1L, n_groups), grouped))
  }

  # With no groups, the expressions are evaluated once on the empty data to find the type of each result
  no_groups <- n_groups == 0L
  if (no_groups) rows <- list(integer())

  chunks <- if (grouped) split_columns(.data, rows) else lapply(.data, list)
  col_names <- names(.data)
  row_names <- .row_names_info(.data, 0L)
  auto_row_names <- .row_names_info(.data) < 0L

  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- character(length(dots))
  unnamed <- !nzchar(dot_names)
  labels <- dot_names
  labels[unnamed] <- vapply(dots[unnamed], auto_label, NA_character_)

  # Restore any outer context so that a summarise() inside another verb does not break it
  outer <- get0(".data", envir = context, inherits = FALSE)
  on.exit(if (is.null(outer)) context$clean() else context$.data <- outer, add = TRUE)

  values <- vector(mode = "list", length = length(rows))
  sizes <- integer(length(rows))
  for (g in seq_along(rows)) {
    cols <- lapply(chunks, .subset2, g)
    slice <- .data
    if (grouped) {
      slice <- cols
      attributes(slice) <- list(
        names = col_names,
        class = "data.frame",
        row.names = if (auto_row_names) as.integer(rows[[g]]) else row_names[rows[[g]]],
        groups = details,
        group_id = if (no_groups) NA_integer_ else g
      )
    }
    context$.data <- slice

    mask <- new.env(parent = env)
    mask$.data <- slice
    list2env(cols, envir = mask)

    res <- vector(mode = "list", length = length(dots))
    size <- 1L
    size_from <- NULL
    for (i in seq_along(dots)) {
      value <- eval(dots[[i]], mask)
      # As in dplyr, NULL results are ignored
      if (is.null(value)) next
      res[i] <- list(value)
      if (unnamed[[i]] && is.data.frame(value)) {
        list2env(value, envir = mask)
      } else {
        assign(labels[[i]], value, envir = mask)
      }
      value_size <- NROW(value)
      if (value_size != 1L && value_size != size) {
        if (size != 1L) {
          stop(
            sprintf("`%s` must be size %d or 1, not %d.", labels[[i]], size, value_size),
            if (!is.null(size_from)) sprintf(" An earlier column, `%s`, had size %d.", size_from, size),
            if (grouped && !no_groups) sprintf(" The error occurred in group %d.", g),
            call. = FALSE
          )
        }
        size <- value_size
        size_from <- labels[[i]]
      }
    }
    values[[g]] <- res
    sizes[[g]] <- size
  }
  # As in dplyr, data with no groups counts as every group giving one row
  all_one <- no_groups || all(sizes == 1L)
  if (no_groups) sizes <- 0L

  out <- list()
  for (i in seq_along(dots)) {
    if (grouped) {
      vals <- lapply(values, .subset2, i)
      is_null <- vapply(vals, is.null, NA)
      if (all(is_null)) next
      if (any(is_null)) {
        stop(
          sprintf("`%s` must return compatible vectors across groups: ", labels[[i]]),
          "it can't combine NULL and non NULL results.",
          call. = FALSE
        )
      }
      recycle <- which(vapply(vals, NROW, 1L) != sizes)
      vals[recycle] <- lapply(recycle, function(g) vec_slice(vals[[g]], rep(1L, sizes[[g]])))
      col <- combine_summaries(vals)
    } else {
      col <- values[[1L]][[i]]
      if (is.null(col)) next
      if (NROW(col) != sizes) col <- vec_slice(col, rep(1L, sizes))
    }
    if (is.data.frame(col)) attr(col, "row.names") <- .set_row_names(nrow(col))
    if (unnamed[[i]] && is.data.frame(col)) {
      for (j in names(col)) out[[j]] <- .subset2(col, j)
    } else {
      out[[labels[[i]]]] <- col
    }
  }

  res <- summarise_result(if (grouped) as.list(details)[groups], out, sizes, grouped)
  res$all_one <- all_one
  res
}

# Join the grouping columns, repeated for each row of their group, to the summaries. As in dplyr, a summary with the
# name of a grouping column replaces it.
summarise_result <- function(keys, cols, sizes, grouped) {
  if (!grouped && length(cols) == 0L) return(list(data = empty_summary(), all_one = TRUE))
  all_one <- all(sizes == 1L)
  if (!all_one) {
    idx <- rep.int(seq_along(sizes), sizes)
    keys <- lapply(keys, function(key) key[idx])
  }
  for (nm in names(cols)) keys[[nm]] <- cols[[nm]]
  if (is.null(keys)) keys <- list()
  n <- if (grouped) sum(sizes) else sizes[[1L]]
  attributes(keys) <- list(names = as.character(names(keys)), class = "data.frame", row.names = .set_row_names(n))
  list(data = keys, all_one = all_one)
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

# Combine the results of a summary across the groups. Results without attributes are combined directly; other common
# types use `combine_columns()`; data frames and matrices are bound by row; anything else falls back to `rbind()`.
combine_summaries <- function(x) {
  if (all(lengths(lapply(x, attributes)) == 0L)) {
    is_atomic <- vapply(x, is.atomic, NA)
    if (all(is_atomic) || !any(is_atomic) && all(vapply(x, is.list, NA))) {
      return(unlist(x, recursive = FALSE, use.names = FALSE))
    }
  }
  if (all(vapply(x, is_simple_column, NA))) {
    res <- combine_columns(x)
    if (!is.null(res)) return(res)
  }
  if (all(vapply(x, is.data.frame, NA))) return(do.call(rbind, unname(x)))
  if (all(vapply(x, is.matrix, NA))) return(do.call(rbind, unname(x)))
  x <- lapply(x, function(v) structure(list(v), names = "x", class = "data.frame", row.names = .set_row_names(NROW(v))))
  .subset2(do.call(rbind, x), 1L)
}

summarise_regroup <- function(x, groups, drop) {
  class(x) <- c("grouped_df", "data.frame")
  groups_set(x, groups, drop)
}

# As in dplyr, a summary with no columns has one row
empty_summary <- function() {
  structure(list(), names = character(0), row.names = .set_row_names(1L), class = "data.frame")
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
