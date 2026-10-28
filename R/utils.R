check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

seq2 <- function(from, to) {
  if (length(from) != 1) stop("`from` must be length one")
  if (length(to) != 1) stop("`to` must be length one")
  if (from > to) integer() else seq.int(from, to)
}

collapse_to_sentence <- function(x) {
  len_x <- length(x)
  if (len_x == 0L) {
    stop("Length of `x` is 0")
  } else if (len_x == 1L) {
    as.character(x)
  } else if (len_x == 2L) {
    paste(x, collapse = " and ")
  } else {
    paste(paste(x[1:(len_x - 1)], collapse = ", "), x[len_x], sep = " and ")
  }
}

#' Check whether any elements of a list are nested
#' @param lst A `list()`
#' @examples
#' is_nested(list(a = 1, b = 2, c = 3))
#' is_nested(list(a = 1, b = list(c = 2, d = 3)))
#' @noRd
is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

#' Remove all levels of a list
#' @noRd
squash <- function(lst) {
  do.call(c, lapply(lst, function(x) if (is.list(x) && !is.data.frame(x)) squash(x) else list(x)))
}

#' Move entries within a list up one level
#' @noRd
flatten <- function(lst) {
  nested <- is_nested(lst)
  res <- c(lst[!nested], unlist(lst[nested], recursive = FALSE))
  if (sum(nested)) Recall(res) else return(res)
}

#' Subset a vector, keeping its attributes
#'
#' Unlike `[`, this keeps all of the attributes of `x` (e.g. custom classes and attributes), except names.
#'
#' @param x A vector, `matrix` or `data.frame`.
#' @param i `integer()`. The positions to keep. `NA` positions return missing values.
#'
#' @noRd
vec_slice <- function(x, i) {
  if (!is.null(dim(x))) return(x[i, , drop = FALSE])
  res <- x[i]
  attrs <- attributes(x)
  if (!is.null(attrs) && is.atomic(x)) {
    attrs$names <- NULL
    attributes(res) <- attrs
  }
  res
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

#' Repair column names
#'
#' @param names `character()`. The names to repair.
#' @param repair Either a `character(1)`, one of `"check_unique"`, `"unique"`, `"universal"` or `"minimal"`, or a
#' function which is applied to `names`.
#' @param error_msg `character(1)`. The error message to use when `repair = "check_unique"` and the names are not
#' unique.
#'
#' @noRd
repair_names <- function(names, repair = "check_unique", error_msg = NULL) {
  names[is.na(names)] <- ""
  if (is.function(repair) || is_formula(repair)) return(as_function(repair)(names))
  repair <- match.arg(repair, c("check_unique", "unique", "universal", "minimal"))
  switch(
    repair,
    minimal = names,
    check_unique = {
      bad <- unique(c(names[names == ""], names[duplicated(names)]))
      if (length(bad) > 0L) {
        if (is.null(error_msg)) {
          error_msg <- paste0(
            "Column names ", paste0("`", bad, "`", collapse = ", "), " must not be duplicated."
          )
        }
        stop(error_msg, "\nUse `names_repair` to choose another strategy.", call. = FALSE)
      }
      names
    },
    unique = report_repair(names, unique_names(names)),
    universal = report_repair(names, unique_names(names, syntactic = TRUE))
  )
}

unique_names <- function(names, syntactic = FALSE) {
  new <- sub("(\\.\\.\\.\\d*|^\\.\\.\\d+)$", "", names)
  if (syntactic) new[new != ""] <- syntactic_names(new[new != ""])
  dups <- new %in% new[duplicated(new)] & new != ""
  new[dups] <- paste0(new[dups], "...", which(dups))
  empty <- new == ""
  new[empty] <- paste0("...", which(empty))
  new
}

syntactic_names <- function(names) {
  names[names == ""] <- "."
  names[names == "..."] <- "...."
  names <- sub("^_", "._", names)
  new <- make.names(names)
  x_prefix <- grepl("^X", new) & !grepl("^X", names)
  new[x_prefix] <- sub("^X", "", new[x_prefix])
  dot_suffix <- which(new == paste0(names, "."))
  new[dot_suffix] <- sub("^(.*)[.]$", ".\\1", new[dot_suffix])
  dots <- sub("^([.]{0,2}).*$", "\\1", new)
  rest <- substr(new, nchar(dots) + 1L, nchar(new))
  numbers <- sub("^([0-9]*).*$", "\\1", rest)
  leftovers <- substr(rest, nchar(numbers) + 1L, nchar(rest))
  needs_dots <- numbers != ""
  dots[needs_dots] <- ifelse(leftovers[needs_dots] == "", "...", "..")
  paste0(dots, numbers, leftovers)
}

report_repair <- function(old, new) {
  changed <- old != new
  if (any(changed)) {
    message("New names:\n", paste0("* `", old[changed], "` -> `", new[changed], "`", collapse = "\n"))
  }
  new
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
  type_name <- function(y) if (is.factor(y)) "factor" else if (is.object(y)) class(y)[1L] else typeof(y)
  fail <- function(reason = "") {
    stop("Can't convert `", x_arg, "` <", type_name(x), "> to <", type_name(to), ">", reason, ".", call. = FALSE)
  }
  is_plain <- function(y) is.atomic(y) && !is.object(y)
  # Vectors which are entirely `NA` can be cast to any type
  if (is.logical(x) && !is.object(x) && all(is.na(x))) return(vec_slice(to, rep(NA_integer_, length(x))))
  if (is.factor(to)) {
    if (!(is.factor(x) || (is_plain(x) && is.character(x)))) fail()
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
    if (!(is_plain(x) && is.character(x))) fail()
    return(as.vector(x))
  }
  if (is.list(to)) {
    if (!is.list(x) || is.object(x)) fail()
    return(x)
  }
  if (!(is_plain(x) && (is.logical(x) || is.numeric(x)))) fail()
  res <- as.vector(x, mode = typeof(to))
  lossy <- !is.na(x) & (is.na(res) | res != x)
  if (any(lossy)) fail(" due to loss of precision")
  res
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

#' Build a data.frame from a list of columns without copying or checking them
#' @param x A named `list` of columns, each of length `n`.
#' @param n `integer(1)`. The number of rows.
#' @noRd
new_data_frame <- function(x, n) {
  structure(x, class = "data.frame", row.names = .set_row_names(n))
}
