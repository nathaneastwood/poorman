#' Pivot data from wide to long
#'
#' `pivot_longer()` "lengthens" data, increasing the number of rows and decreasing the number of columns. The inverse
#' transformation is [pivot_wider()].
#'
#' @param data `data.frame`. The data to pivot.
#' @param cols <[`poor-select`][select_helpers]>. Columns to pivot into longer format.
#' @param names_to `character(n)`. The name of the new column(s) that will contain the column names. If of length > 1,
#' one of `names_sep` or `names_pattern` must be supplied. Two values have special meaning: `NA` discards the
#' corresponding piece of the column name and `".value"` indicates that the corresponding piece of the column name
#' defines the name of the output column containing the values, overriding `values_to`.
#' @param names_prefix `character(1)`. A regular expression used to remove matching text from the start of each variable
#' name.
#' @param names_sep,names_pattern `character(1)`. If `names_to` contains multiple values, this argument controls how the
#' column name is broken up. `names_sep` is a fixed string on which to split the column names. `names_pattern` takes a
#' regular expression containing matching groups (`()`); with a single value of `names_to`, the first matching group
#' is used.
#' @param values_to `character(1)`. The name of the new column that will contain the values of the pivoted variables.
#' The pivoted columns are combined following the `tidyr` rules: logical, integer and double columns are combined into
#' the richest of those types, factors are combined into a factor with the union of their levels, factors and
#' characters are combined into a character and columns of the same class keep that class. Other combinations are an
#' error.
#' @param names_ptypes,values_ptypes `list()`. Optionally, a named list of column name-prototype pairs, e.g.
#' `list(week = factor())`, used to cast the newly created columns. Casts that lose information are an error.
#' @param names_transform,values_transform `list()`. Optionally, a named list of column name-function pairs, e.g.
#' `list(week = as.integer)`, applied to the newly created columns. Functions may be given as formulas, e.g.
#' `~ as.integer(.x)`.
#' @param names_repair `character(1)` or `function`. What happens if the output has invalid column names. The default,
#' `"check_unique"`, is to error if the columns are duplicated. Use `"minimal"` to allow duplicates in the output,
#' `"unique"` to de-duplicate by adding numeric suffixes or `"universal"` to also make the names syntactic. A function
#' is applied to the names.
#' @param values_drop_na `logical(1)`. If `TRUE`, will drop rows that contain only `NA` in the `values_to` column. This
#' effectively converts explicit missing values to implicit missing values, and should generally be used only when
#' missing values in data were created by its structure.
#' @param ... Additional arguments passed on to methods.
#'
#' @return A `data.frame`.
#'
#' @examples
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#' # Customizing the names
#' pivot_longer(
#'   data = wide_data,
#'   cols = c(1, 2),
#'   names_to = "Column",
#'   values_to = "Numbers"
#' )
#'
#' @export
pivot_longer <- function(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = list(),
  names_transform = list(),
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = list(),
  values_transform = list(),
  ...
) {

  if (missing(cols)) {
    stop("`cols` must select at least one column.")
  }

  col_pos <- unname(eval_select_pos(data, substitute(cols)))
  # Columns sharing a name are all pivoted
  if (anyDuplicated(names(data))) col_pos <- which(names(data) %in% names(data)[col_pos])

  if (length(col_pos) == 0L) {
    stop("No columns found for reshaping data.", call. = FALSE)
  }
  if (!(".value" %in% names_to) && (!is.character(values_to) || length(values_to) != 1L)) {
    stop("`values_to` must be a single string.", call. = FALSE)
  }

  cols <- names(data)[col_pos]
  id_pos <- setdiff(seq_along(data), col_pos)
  id_cols <- names(data)[id_pos]

  if (identical(names_repair, "check_unique") && any(names_to %in% id_cols)) {
    stop(
      paste0(
        "Some values of the columns specified in 'names_to' are already present
        as column names. Either use another value in `names_to` or rename the
        following columns: ",
        paste(names_to[which(names_to %in% id_cols)], sep = ", ")
      ),
      call. = FALSE)
  }

  spec <- build_longer_spec(
    cols, names_to, names_prefix, names_sep, names_pattern, values_to, names_ptypes, names_transform
  )
  n_row <- nrow(data)

  # Each unique combination of the `names_to` columns becomes one row of output per input row. Columns which share
  # their `.value` and `names_to` values are told apart by a sequence number, as in tidyr.
  key_names <- setdiff(names(spec), c(".name", ".value"))
  keys <- as.list(spec)[key_names]
  dup_id <- vec_group_id(c(list(spec$.value), keys), n = nrow(spec))
  if (anyDuplicated(dup_id)) keys$.seq <- seq_by_group(dup_id)
  key_id <- vec_group_id(keys, n = nrow(spec))
  key_rows <- first_positions(key_id)
  n_key <- length(key_rows)

  values <- unique(spec$.value)
  vals <- lapply(values, function(value) {
    in_value <- which(spec$.value == value)
    val_cols <- rep(list(rep(NA, n_row)), n_key)
    val_cols[key_id[in_value]] <- lapply(col_pos[in_value], function(i) data[[i]])
    val_names <- rep("", n_key)
    val_names[key_id[in_value]] <- spec$.name[in_value]
    if (is.list(values_transform) && !is.null(values_transform[[value]])) {
      val_cols <- lapply(val_cols, as_function(values_transform[[value]]))
    }
    if (!is.null(values_ptypes[[value]])) {
      val_cols <- mapply(
        cast_ptype, val_cols, list(values_ptypes[[value]]), val_names, SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
    }
    names(val_cols) <- val_names
    interleave(combine_columns(val_cols), n_row)
  })
  names(vals) <- values

  out <- c(
    lapply(as.list(data)[id_pos], function(x) {
      if (is.null(attributes(x))) rep(x, each = n_key) else vec_slice(x, rep(seq_len(n_row), each = n_key))
    }),
    lapply(spec[key_rows, key_names, drop = FALSE], function(x) {
      if (is.null(attributes(x))) rep(x, times = n_row) else vec_slice(x, rep(seq_len(n_key), times = n_row))
    }),
    vals
  )
  names(out) <- repair_names(names(out), names_repair)

  if (values_drop_na) {
    missing <- Reduce(`&`, lapply(vals, is.na))
    if (any(missing)) out <- lapply(out, vec_slice, which(!missing))
  }

  out <- structure(out, class = "data.frame", row.names = .set_row_names(length(out[[length(out)]])))
  reconstruct_attrs(out, data)
}

# The position of each element within its group, e.g. c(1, 1, 2, 1) gives c(1, 2, 1, 3)
seq_by_group <- function(group) {
  res <- integer(length(group))
  res[order(group)] <- sequence(tabulate(group))
  res
}

# Build a data.frame describing how each pivoted column maps to the output: its name (`.name`), the column of values it
# belongs to (`.value`) and the values of the new `names_to` columns
build_longer_spec <- function(
  cols, names_to, names_prefix, names_sep, names_pattern, values_to, names_ptypes = list(), names_transform = list()
) {
  names <- cols
  if (!is.null(names_prefix)) names <- sub(paste0("^", names_prefix), "", names)

  if (length(names_to) > 1L) {
    if (!xor(is.null(names_sep), is.null(names_pattern))) {
      stop(
        "If you supply multiple names in `names_to` you must also supply one of `names_sep` or `names_pattern`.",
        call. = FALSE
      )
    }
    pieces <- if (!is.null(names_sep)) {
      split_names(names, names_sep, length(names_to))
    } else {
      extract_names(names, names_pattern, length(names_to))
    }
  } else if (length(names_to) == 0L) {
    pieces <- list()
  } else {
    if (!is.null(names_sep)) {
      stop("`names_sep` can not be used with length-1 `names_to`.", call. = FALSE)
    }
    pieces <- list(if (is.null(names_pattern)) names else extract_names(names, names_pattern, 1L)[[1L]])
  }
  names(pieces) <- names_to
  pieces <- pieces[!is.na(names_to)]
  if (anyDuplicated(names(pieces))) {
    dups <- unique(names(pieces)[duplicated(names(pieces))])
    stop("Column name ", paste0("`", dups, "`", collapse = ", "), " must not be duplicated.", call. = FALSE)
  }
  for (piece in intersect(names(pieces), names(names_ptypes))) {
    pieces[[piece]] <- cast_ptype(pieces[[piece]], names_ptypes[[piece]], piece)
  }
  for (piece in intersect(names(pieces), names(names_transform))) {
    pieces[[piece]] <- as_function(names_transform[[piece]])(pieces[[piece]])
  }

  spec <- structure(list(.name = cols), class = "data.frame", row.names = .set_row_names(length(cols)))
  if (".value" %in% names(pieces)) {
    spec$.value <- pieces$.value
    pieces$.value <- NULL
  } else {
    spec$.value <- rep(values_to, length(cols))
  }
  if (any(names(pieces) %in% c(".name", ".value"))) {
    stop("`names_to` must not contain `.name` or more than one `.value`.", call. = FALSE)
  }
  for (piece in names(pieces)) spec[[piece]] <- pieces[[piece]]
  spec
}

# Split strings on a fixed separator into `n` pieces, padding with `NA`
split_names <- function(x, sep, n) {
  split <- strsplit(x, sep, fixed = TRUE)
  lens <- lengths(split)
  if (any(lens > n)) {
    warning(
      "Expected ", n, " pieces. Additional pieces discarded in ", sum(lens > n), " rows [",
      paste(which(lens > n), collapse = ", "), "].",
      call. = FALSE
    )
  }
  if (any(lens < n)) {
    warning(
      "Expected ", n, " pieces. Missing pieces filled with `NA` in ", sum(lens < n), " rows [",
      paste(which(lens < n), collapse = ", "), "].",
      call. = FALSE
    )
  }
  lapply(seq_len(n), function(i) vapply(split, `[`, NA_character_, i))
}

# Extract the matching groups of a regular expression into `n` pieces, using `NA` where there is no match
extract_names <- function(x, pattern, n) {
  matches <- regmatches(x, regexec(pattern, x))
  n_groups <- max(lengths(matches)) - 1L
  if (n_groups < 0L) n_groups <- length(regmatches(pattern, gregexpr("\\((?!\\?)", pattern, perl = TRUE))[[1L]])
  if (n_groups != n) {
    stop("`regex` should define ", n, " groups; ", n_groups, " found.", call. = FALSE)
  }
  matches[lengths(matches) == 0L] <- list(rep(NA_character_, n + 1L))
  lapply(seq_len(n) + 1L, function(i) vapply(matches, `[`, NA_character_, i))
}

# Combine a list of columns into a single vector following the `tidyr` type rules: logical, integer and double
# columns are combined into the richest of those types, factors into a factor with the union of their levels, factors
# and characters into a character and columns of the same class keep that class. Columns which are entirely `NA` are
# compatible with any type. Other combinations are an error.
combine_columns <- function(cols) {
  unspecified <- vapply(cols, function(x) is.logical(x) && is.null(attributes(x)) && all(is.na(x)), NA)
  if (all(unspecified)) return(unlist(cols, use.names = FALSE))
  typed <- cols[!unspecified]
  template <- typed[[1L]]
  cols[unspecified] <- lapply(cols[unspecified], function(x) vec_slice(template, rep(NA_integer_, length(x))))

  is_plain <- function(x) is.atomic(x) && !is.object(x) && is.null(dim(x))
  same_attrs <- function(x) identical(attributes(x)[names(attributes(x)) != "names"], attrs)
  attrs <- attributes(template)
  attrs <- attrs[names(attrs) != "names"]

  if (all(vapply(typed, is_plain, NA))) {
    if (all(vapply(typed, is.character, NA)) || !any(vapply(typed, is.character, NA))) {
      return(unlist(lapply(cols, as.vector), use.names = FALSE))
    }
  } else if (all(vapply(typed, is.factor, NA))) {
    if (all(vapply(typed, same_attrs, NA))) {
      res <- unlist(lapply(cols, as.integer), use.names = FALSE)
      attributes(res) <- attrs
      return(res)
    }
    lvls <- unique(unlist(lapply(typed, levels), use.names = FALSE))
    return(factor(unlist(lapply(cols, as.character), use.names = FALSE), levels = lvls))
  } else if (all(vapply(typed, function(x) is.factor(x) || (is_plain(x) && is.character(x)), NA))) {
    return(unlist(lapply(cols, as.character), use.names = FALSE))
  } else if (all(vapply(typed, function(x) is.list(x) && !is.object(x), NA))) {
    return(unlist(cols, recursive = FALSE, use.names = FALSE))
  } else if (is.atomic(template) && is.null(dim(template)) && all(vapply(typed, same_attrs, NA))) {
    res <- unlist(lapply(cols, unclass), use.names = FALSE)
    attributes(res) <- attrs
    return(res)
  }

  type_name <- function(x) if (is.object(x)) class(x)[1L] else typeof(x)
  types <- vapply(typed, type_name, NA_character_)
  other <- which(types != types[1L])[1L]
  if (is.na(other)) other <- 2L
  stop(
    "Can't combine `", names(typed)[1L], "` <", types[1L], "> and `", names(typed)[other], "` <", types[other], ">.",
    call. = FALSE
  )
}

# Reorder a vector made up of `length(x) / n` blocks of length `n` so that the first element of each block comes
# first, then the second element of each block and so on
interleave <- function(x, n) {
  if (n == 0L || length(x) == n) return(x)
  k <- length(x) %/% n
  if (is.atomic(x)) {
    attrs <- attributes(x)
    attributes(x) <- NULL
    res <- as.vector(t(matrix(x, nrow = n, ncol = k)))
    attributes(res) <- attrs
    res
  } else {
    x[as.vector(t(matrix(seq_along(x), nrow = n, ncol = k)))]
  }
}
