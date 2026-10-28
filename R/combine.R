#' Combine the values of each group into one vector
#'
#' Values without attributes are combined directly; `POSIXct`s, with any `Date`s, use `combine_datetimes()`; factors,
#' plain `Date`s and unclassed lists use `combine_columns()`, so they follow the type rules of `pivot_longer()` and
#' incompatible types are an error; data frames and matrices are bound by row; anything else falls back to `rbind()`.
#'
#' @param x A `list` of values, one for each group, none of them `NULL`.
#' @return The combined values.
#' @noRd
combine_values <- function(x) {
  if (is_plain_values(x)) return(unlist(x, recursive = FALSE, use.names = FALSE))
  if (is_datetime_mix(x)) return(combine_datetimes(x))
  if (all(vapply(x, is_simple_column, NA))) return(combine_columns(x))
  if (all(vapply(x, is.data.frame, NA))) return(combine_data_frames(x))
  if (all(vapply(x, is.matrix, NA))) return(do.call(rbind, unname(x)))
  x <- lapply(x, function(v) new_data_frame(list(x = v), NROW(v)))
  .subset2(do.call(rbind, x), 1L)
}

#' Bind data frames by row, for `combine_values()`
#'
#' Data frames such as the results of `across()`, with the same names, automatic row names and columns without
#' attributes, are bound column by column with `unlist()`, which is much faster than `rbind()` when there are many
#' groups and gives the same result. Any other data frames are bound with `rbind()`.
#'
#' @param x A `list` of data frames, one for each group.
#' @return A `data.frame`.
#' @noRd
combine_data_frames <- function(x) {
  x <- unname(x)
  nms <- names(x[[1L]])
  simple <- all(vapply(
    x,
    function(df) identical(class(df), "data.frame") && identical(names(df), nms) && .row_names_info(df) <= 0L,
    NA
  ))
  if (simple) {
    cols <- lapply(seq_along(nms), function(j) lapply(x, .subset2, j))
    # unlist() would also combine types which rbind() does not, e.g. raw and integer, so those are left to rbind()
    simple <- all(vapply(cols, function(vals) {
      types <- plain_types(vals)
      is_one_type(types) && types[[1L]] != "list"
    }, NA))
  }
  if (!simple) {
    res <- do.call(rbind, x)
    # rbind() gives a matrix column empty dimnames, which a matrix result of an expression does not have
    for (j in which(vapply(res, function(col) is.matrix(col) && all(vapply(dimnames(col), is.null, NA)), NA))) {
      dimnames(res[[j]]) <- NULL
    }
    return(res)
  }
  cols <- lapply(cols, unlist, recursive = FALSE, use.names = FALSE)
  names(cols) <- nms
  new_data_frame(cols, sum(vapply(x, .row_names_info, 1L, type = 2L)))
}

#' Find the types of values without attributes
#'
#' @param x A `list` of values.
#' @return `character(n)`. The `typeof()` of each value, or `NA` where the value has attributes.
#' @noRd
plain_types <- function(x) {
  vapply(x, function(v) if (is.null(attributes(v))) typeof(v) else NA_character_, NA_character_)
}

#' Check whether types are all the same, or all logical, integer or double
#'
#' @param types `character()`, as returned by `plain_types()`.
#' @return `logical(1)`. `FALSE` if any type is `NA`.
#' @noRd
is_one_type <- function(types) {
  types <- unique(types)
  !anyNA(types) && (length(types) == 1L || all(types %in% c("logical", "integer", "double")))
}

#' Check whether values can be combined with `unlist()`, for `combine_values()`
#'
#' @param x A `list` of values, one for each group.
#' @return `logical(1)`. `TRUE` if no value has attributes and the values are all atomic vectors or all lists.
#' @noRd
is_plain_values <- function(x) {
  if (!all(lengths(lapply(x, attributes)) == 0L)) return(FALSE)
  is_atomic <- vapply(x, is.atomic, NA)
  all(is_atomic) || !any(is_atomic) && all(vapply(x, is.list, NA))
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

#' Check whether values are datetimes, possibly with dates, for `combine_datetimes()`
#'
#' @param x A `list` of values, one for each group.
#' @return `logical(1)`. `TRUE` if at least one value is a `POSIXct` and the others are `POSIXct`, `Date` or logical
#' vectors of only `NA`.
#' @noRd
is_datetime_mix <- function(x) {
  any(vapply(x, inherits, NA, "POSIXct")) &&
    all(vapply(x, function(v) inherits(v, c("Date", "POSIXct")) || is_all_na_logical(v), NA))
}

#' Combine dates and datetimes into one datetime vector
#'
#' The result has the time zone of the first datetime which has one, or the local time zone if none has,
#' and each date is the midnight that starts it in that time zone, so that no datetime loses its time.
#'
#' @param x A `list` of `POSIXct` and `Date` vectors and logical vectors of only `NA`, at least one of them `POSIXct`.
#' @return A `POSIXct` vector.
#' @noRd
combine_datetimes <- function(x) {
  zones <- unlist(lapply(x, function(v) if (inherits(v, "POSIXct")) attr(v, "tzone", exact = TRUE)[1L]))
  zones <- zones[!is.na(zones) & nzchar(zones)]
  tz <- if (length(zones) > 0L) zones[[1L]] else ""
  secs <- lapply(x, function(v) {
    if (inherits(v, "Date")) return(as.numeric(as.POSIXct(format(v), tz = tz)))
    if (is.logical(v)) return(rep(NA_real_, length(v)))
    as.numeric(v)
  })
  structure(unlist(secs, use.names = FALSE), class = c("POSIXct", "POSIXt"), tzone = tz)
}

#' Combine a list of columns into one vector
#'
#' Logical, integer and double columns are combined into the richest of those types,
#' factors into a factor with the union of their levels, factors and characters into a character and columns of the
#' same class keep that class. Columns which are entirely `NA` are compatible with any type. Other combinations are an
#' error. Used by `pivot_longer()`, `pivot_wider()` and, through `combine_values()`, the grouped verbs.
#'
#' @param cols A `list` of vectors, optionally named; the names are used in the error message.
#' @return The combined vector.
#' @noRd
combine_columns <- function(cols) {
  unspecified <- vapply(cols, function(x) is.logical(x) && is.null(attributes(x)) && all(is.na(x)), NA)
  if (all(unspecified)) return(unlist(cols, use.names = FALSE))
  typed <- cols[!unspecified]
  template <- typed[[1L]]
  cols[unspecified] <- lapply(cols[unspecified], function(x) vec_slice(template, rep(NA_integer_, length(x))))

  is_plain <- function(x) is_bare_atomic(x) && is.null(dim(x))
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

  types <- vapply(typed, class_name, NA_character_)
  other <- which(types != types[1L])[1L]
  if (is.na(other)) other <- 2L
  stop(
    "Can't combine `", names(typed)[1L], "` <", types[1L], "> and `", names(typed)[other], "` <", types[other], ">.",
    call. = FALSE
  )
}

#' Cast a vector to the type of a prototype
#'
#' Casts between logical, integer and double when no information is lost,
#' between character and factor, and between objects of the same class. Other casts are an error.
#'
#' @param x The vector to cast.
#' @param to The prototype, e.g. `integer()` or `factor(levels = c("a", "b"))`.
#' @param x_arg `character(1)`. The name of `x`, used in error messages.
#'
#' @noRd
cast_ptype <- function(x, to, x_arg = "x") {
  # Any factor, ordered or not, is cast in the same way, so it is named as a factor
  type_name <- function(y) if (is.factor(y)) "factor" else class_name(y)
  fail <- function(reason = "") {
    stop("Can't convert `", x_arg, "` <", type_name(x), "> to <", type_name(to), ">", reason, ".", call. = FALSE)
  }
  # Vectors which are entirely `NA` can be cast to any type
  if (is.logical(x) && !is.object(x) && all(is.na(x))) return(vec_slice(to, rep(NA_integer_, length(x))))
  if (is.factor(to)) {
    if (!(is.factor(x) || (is_bare_atomic(x) && is.character(x)))) fail()
    x_chr <- as.character(x)
    lvls <- if (length(levels(to)) == 0L) unique(x_chr[!is.na(x_chr)]) else levels(to)
    if (!all(is.na(x_chr) | x_chr %in% lvls)) fail(" due to loss of generality")
    return(structure(match(x_chr, lvls), levels = lvls, class = class(to)))
  }
  if (is.object(to)) {
    if (!identical(class(x), class(to))) fail()
    return(x)
  }
  if (is.character(to)) {
    if (is.factor(x)) return(as.character(x))
    if (!(is_bare_atomic(x) && is.character(x))) fail()
    return(as.vector(x))
  }
  if (is.list(to)) {
    if (!is.list(x) || is.object(x)) fail()
    return(x)
  }
  if (!(is_bare_atomic(x) && (is.logical(x) || is.numeric(x)))) fail()
  res <- as.vector(x, mode = typeof(to))
  lossy <- !is.na(x) & (is.na(res) | res != x)
  if (any(lossy)) fail(" due to loss of precision")
  res
}

#' Is a vector a logical vector of only missing values?
#'
#' Such a vector has no type of its own and can be combined with a vector of any type, taking that type.
#' An empty, classed or matrix logical vector does not count.
#'
#' @param x A vector.
#' @return `logical(1)`.
#' @noRd
is_all_na_logical <- function(x) {
  is.logical(x) && !is.object(x) && is.null(dim(x)) && length(x) > 0L && all(is.na(x))
}

#' Find the kind of a result, for `check_group_types()`
#'
#' Results of the same kind can be combined: logical, integer, double and complex vectors, see `find_type_clash()`;
#' factors and character vectors; `Date`
#' and `POSIXct` vectors; unclassed lists; data frames; or objects of the same class. A matrix or array is of the kind
#' of its base type.
#'
#' @param x A result of an expression for one group.
#' @return `character(1)`, or `""` for a logical vector of only `NA`, which can be combined with any kind.
#' @noRd
result_kind <- function(x) {
  if (is_all_na_logical(x)) return("")
  if (is.data.frame(x)) return("data.frame")
  if (is.factor(x)) return("character")
  if (inherits(x, c("Date", "POSIXct"))) return("datetime")
  if (is.object(x)) return(paste(class(x), collapse = "/"))
  # A matrix is not a kind of its own: a vector of the same base type is combined with it, each element filling a row
  type <- typeof(x)
  if (type %in% c("logical", "integer", "double", "complex")) "number" else type
}

#' Find the first result which cannot be combined with the results of the groups before it
#'
#' The groups are followed in order, and the type of the results so far is given by the earliest group
#' whose result raised it, e.g. from integer to double; a logical vector of only `NA` takes the type of the result that
#' follows it. Durations of different units combine to seconds, but keep the earlier group.
#'
#' @param vals A `list` with the result of the expression for each group.
#' @param kinds `character(n)`. The kind of each result, as given by `result_kind()`.
#' @return `NULL` if every result can be combined, otherwise a `list` with `ref`, the group which gave the type of the
#' results so far, `ref_type`, that type, `g`, the group of the incompatible result, and `type`, its type.
#' @noRd
find_type_clash <- function(vals, kinds) {
  ref <- 1L
  ref_kind <- kinds[[1L]]
  ref_type <- type_label(vals[[1L]])
  for (g in seq_along(vals)[-1L]) {
    if (!nzchar(kinds[[g]])) next
    type <- type_label(vals[[g]])
    if (!nzchar(ref_kind)) {
      ref_kind <- kinds[[g]]
      ref_type <- type
      next
    }
    if (kinds[[g]] != ref_kind || is_complex_logical(ref_type, type)) {
      return(list(ref = ref, ref_type = ref_type, g = g, type = type))
    }
    common <- richer_type(ref_type, type)
    if (common != ref_type) {
      if (!startsWith(common, "duration<")) ref <- g
      ref_type <- common
    }
  }
  NULL
}

# Are two types, as given by `type_label()`, a complex and a logical, which are not combined?
is_complex_logical <- function(a, b) {
  setequal(sub("[[].*", "", c(a, b)), c("complex", "logical"))
}

#' Name the type of a result in an error message
#'
#' Base types are given by `typeof()`, `Date` as `date`, `POSIXct` as `datetime<tz>` and `difftime` as
#' `duration<units>`. A matrix or array adds its dimensions other than the rows, e.g. `integer[,2]`, or
#' `[1d]` if it has one dimension. Other objects are given by their first class.
#'
#' @param x A result of an expression for one group.
#' @return `character(1)`.
#' @noRd
type_label <- function(x) {
  if (is.object(x)) return(object_type_label(x))
  dims <- dim(x)
  if (length(dims) == 1L) return(paste0(typeof(x), "[1d]"))
  if (length(dims) > 1L) return(sprintf("%s[,%s]", typeof(x), paste(dims[-1L], collapse = ",")))
  typeof(x)
}

#' Name the type of a result with a class in an error message, for `type_label()`
#'
#' @param x A result of an expression for one group, with a class.
#' @return `character(1)`.
#' @noRd
object_type_label <- function(x) {
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone", exact = TRUE)[1L]
    return(sprintf("datetime<%s>", if (is.null(tz) || is.na(tz) || !nzchar(tz)) "local" else tz))
  }
  if (inherits(x, "difftime")) return(sprintf("duration<%s>", attr(x, "units", exact = TRUE)))
  class(x)[1L]
}

#' Find the type of two combined results of the same kind
#'
#' @param a,b `character(1)`. The types of the results, as given by `type_label()`.
#' @return `character(1)`. The type of the combined results.
#' @noRd
richer_type <- function(a, b) {
  # A matrix combined with a vector keeps its dimensions, e.g. an integer matrix and a double give `double[,2]`
  dims <- sub("^[^[]*", "", c(a, b))
  dims <- c(dims[nzchar(dims)], "")[[1L]]
  a <- sub("[[].*", "", a)
  b <- sub("[[].*", "", b)
  numbers <- c("logical", "integer", "double", "complex")
  type <- if (a %in% numbers && b %in% numbers) {
    numbers[max(match(c(a, b), numbers))]
  } else if (startsWith(a, "duration<") && a != b) {
    # Durations of different units are combined in seconds
    "duration<secs>"
  } else if (b == "character" || a == "date") {
    # A factor combined with a character vector gives a character vector, and a date combined with a datetime a
    # datetime
    b
  } else {
    a
  }
  paste0(type, dims)
}

#' Name the type of a vector in the errors of `cast_ptype()` and `combine_columns()`
#' @param x A vector.
#' @return `character(1)`. The first class of `x`, or its base type if it has no class.
#' @noRd
class_name <- function(x) {
  if (is.object(x)) class(x)[1L] else typeof(x)
}

#' Check whether a vector is atomic and has no class
#' @param x A vector.
#' @return `logical(1)`.
#' @noRd
is_bare_atomic <- function(x) {
  is.atomic(x) && !is.object(x)
}
