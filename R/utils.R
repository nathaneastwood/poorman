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

#' Build a data.frame from a list of columns without copying or checking them
#' @param x A named `list` of columns, each of length `n`. A list with no names is given empty ones, so that a
#' data.frame with no columns still has `names`.
#' @param n `integer(1)`. The number of rows.
#' @noRd
new_data_frame <- function(x, n) {
  if (is.null(names(x))) names(x) <- character(length(x))
  structure(x, class = "data.frame", row.names = .set_row_names(n))
}
