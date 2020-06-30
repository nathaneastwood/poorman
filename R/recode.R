#' Recode values
#'
#' @description
#' This is a vectorised version of [switch()]: you can replace `numeric` values based on their position or their name,
#' and `character` or `factor` values only by their name. This is an S3 generic: `{poorman}` provides methods for
#' `numeric`,  `character`, and `factor`s. For `logical` vectors, use [if_else()].
#'
#' You can use `recode()` directly with `factor`s; it will preserve the existing order of levels while changing the
#' values. Alternatively, you can use `recode_factor()`, which will change the order of levels to match the order of
#' replacements.
#'
#' This is a direct port of the `dplyr::recode()` function.
#'
#' @param .x A vector to modify
#' @param ... Replacements. For `character` and `factor` `.x`, these should be named and replacement is based only on
#' their name. For `numeric` `.x`, these can be named or not. If not named, the replacement is done based on position
#' i.e. `.x` represents positions to look for in replacements. See examples.
#'
#' When named, the argument names should be the current values to be replaced, and the argument values should be the new
#' (replacement) values.
#'
#' All replacements must be the same type, and must have either length one or the same length as `.x`.
#' @param .default If supplied, all values not otherwise matched will be given this value. If not supplied and if the
#' replacements are the same type as the original values in `.x`, unmatched values are not changed. If not supplied and
#' if the replacements are not compatible, unmatched values are replaced with `NA`.
#'
#' `.default` must be either length 1 or the same length as `.x`.
#' @param .missing If supplied, any missing values in `.x` will be replaced by this value. Must be either length 1 or
#' the same length as `.x`.
#' @param .ordered `logical(1)`. If `TRUE`, `recode_factor()` creates an ordered `factor`.
#'
#' @return A vector the same length as `.x`, and the same type as
#'   the first of `...`, `.default`, or `.missing`.
#'   `recode_factor()` returns a factor whose levels are in the same order as
#'   in `...`. The levels in `.default` and `.missing` come last.
#'
#' @seealso
#' [na_if()] to replace specified values with a `NA`.
#'
#' [coalesce()] to replace missing values with a specified value.
#'
#' [replace_na()] to replace `NA` with a value.
#'
#' @examples
#' # For character values, recode values with named arguments only. Unmatched
#' # values are unchanged.
#' char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
#' recode(char_vec, a = "Apple")
#' recode(char_vec, a = "Apple", b = "Banana")
#'
#' # Use .default as replacement for unmatched values. Note that NA and
#' # replacement values need to be of the same type.
#' recode(char_vec, a = "Apple", b = "Banana", .default = NA_character_)
#'
#' # Throws an error as NA is logical, not character.
#' \dontrun{
#' recode(char_vec, a = "Apple", b = "Banana", .default = NA)
#' }
#'
#' # For numeric values, named arguments can also be used
#' num_vec <- c(1:4, NA)
#' recode(num_vec, `2` = 20L, `4` = 40L)
#'
#' # Or if you don't name the arguments, recode() matches by position.
#' # (Only works for numeric vector)
#' recode(num_vec, "a", "b", "c", "d")
#' # .x (position given) looks in (...), then grabs (... value at position)
#' # so if nothing at position (here 5), it uses .default or NA.
#' recode(c(1, 5, 3), "a", "b", "c", "d", .default = "nothing")
#'
#' # Note that if the replacements are not compatible with .x,
#' # unmatched values are replaced by NA and a warning is issued.
#' recode(num_vec, `2` = "b", `4` = "d")
#' # use .default to change the replacement value
#' recode(num_vec, "a", "b", "c", .default = "other")
#' # use .missing to replace missing values in .x
#' recode(num_vec, "a", "b", "c", .default = "other", .missing = "missing")
#'
#' # For factor values, use only named replacements
#' # and supply default with levels()
#' factor_vec <- factor(c("a", "b", "c"))
#' recode(factor_vec, a = "Apple", .default = levels(factor_vec))
#'
#' # Use recode_factor() to create factors with levels ordered as they
#' # appear in the recode call. The levels in .default and .missing
#' # come last.
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x")
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x", .default = "D")
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x", .default = "D", .missing = "M")
#'
#' # When the input vector is a compatible vector (character vector or
#' # factor), it is reused as default.
#' recode_factor(letters[1:3], b = "z", c = "y")
#' recode_factor(factor(letters[1:3]), b = "z", c = "y")
#'
#' @export
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  UseMethod("recode")
}

#' @export
recode.numeric <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- dotdotdot(...)

  nms <- have_name(values)
  if (all(nms)) {
    vals <- as.double(names(values))
  } else if (all(!nms)) {
    vals <- seq_along(values)
  } else {
    stop("Either all values must be named, or none must be named.")
  }

  n <- length(.x)
  template <- find_template(values, .default, .missing)
  res <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (i in seq_along(values)) {
    res <- replace_with(res, .x == vals[i], values[[i]], paste0("Vector ", i))
    replaced[.x == vals[i]] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, res, replaced)
  res <- replace_with(res, !replaced & !is.na(.x), .default, "`.default`")
  res <- replace_with(res, is.na(.x), .missing, "`.missing`")
  res
}

#' @export
recode.character <- function(.x, ..., .default = NULL, .missing = NULL) {
  .x <- as.character(.x)
  values <- dotdotdot(...)
  val_names <- names(values)
  have_names <- have_name(values)
  if (!all(have_names)) {
    bad <- which(!have_names) + 1L
    stop("Argument", if (length(bad) > 1L) "s", " ", paste(bad, sep = ", "), " must be named, not unnamed.")
  }

  n <- length(.x)
  template <- find_template(values, .default, .missing)
  res <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in val_names) {
    res <- replace_with(res, .x == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[.x == nm] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, res, replaced)
  res <- replace_with(res, !replaced & !is.na(.x), .default, "`.default`")
  res <- replace_with(res, is.na(.x), .missing, "`.missing`")
  res
}

#' @export
recode.factor <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- dotdotdot(...)
  if (length(values) == 0) stop("No replacements provided.")

  have_names <- have_name(values)
  if (!all(have_names)) {
    bad <- which(!have_names) + 1
    stop(bad, " must be named, not unnamed.")
  }
  if (!is.null(.missing)) {
    stop("`.missing` is not supported for factors.")
  }

  n <- length(levels(.x))
  template <- find_template(values, .default, .missing)
  res <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in names(values)) {
    res <- replace_with(res, levels(.x) == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[levels(.x) == nm] <- TRUE
  }
  .default <- validate_recode_default(.default, .x, res, replaced)
  res <- replace_with(res, !replaced, .default, "`.default`")

  if (is.character(res)) {
    levels(.x) <- res
    .x
  } else {
    res[as.integer(.x)]
  }
}

have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) rep(FALSE, length(x)) else !(nms == "" | is.na(nms))
}

compact <- function(.x) Filter(length, .x)

find_template <- function(values, .default = NULL, .missing = NULL) {
  x <- compact(c(values, .default, .missing))
  if (length(x) == 0L) {
    stop("No replacements provided.")
  }
  x[[1]]
}

validate_recode_default <- function(default, x, res, replaced) {
  default <- recode_default(x, default, res)
  if (is.null(default) && sum(replaced & !is.na(x)) < length(res[!is.na(x)])) {
    warning(
      "Unreplaced values treated as NA as .x is not compatible. ",
      "Please specify replacements exhaustively or supply .default",
      call. = FALSE
    )
  }
  default
}

recode_default <- function(x, default, res) {
  UseMethod("recode_default")
}

recode_default.default <- function(x, default, res) {
  same_type <- identical(typeof(x), typeof(res))
  if (is.null(default) && same_type) x else default
}

recode_default.factor <- function(x, default, res) {
  if (is.null(default)) {
    if ((is.character(res) || is.factor(res)) && is.factor(x)) {
      levels(x)
    } else {
      res[NA_integer_]
    }
  } else {
    default
  }
}

#' @rdname recode
#' @export
recode_factor <- function(.x, ..., .default = NULL, .missing = NULL, .ordered = FALSE) {
  recoded <- recode(.x, ..., .default = .default, .missing = .missing)

  values <- dotdotdot(...)
  all_levels <- unique(c(values, recode_default(.x, .default, recoded), .missing))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}
