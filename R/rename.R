#' Rename columns
#'
#' `rename()` changes the names of individual variables using `new_name = old_name` syntax.
#' `rename_with()` renames columns using a function.
#'
#' @param .data A `data.frame`
#' @param ...
#'   For `rename()`: comma separated key-value pairs in the form of `new_name = old_name` to rename selected variables.
#'
#'   For `rename_with()`: additional arguments passed onto `.fn`.
#'
#' @return
#' A `data.frame` with the following properties:
#'
#' * Rows are not affected.
#' * Column names are changed; column order is preserved.
#' * `data.frame` attributes are preserved.
#' * Groups are updated to reflect new names.
#'
#' @examples
#' rename(mtcars, MilesPerGallon = mpg)
#' rename(mtcars, Cylinders = cyl, Gears = gear)
#' mtcars %>% rename(MilesPerGallon = mpg)
#'
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' @export
rename.data.frame <- function(.data, ...) {
  new_names <- names(dotdotdot(...))
  if (length(new_names) == 0L) {
    warning("You didn't give any new names")
    return(.data)
  }
  col_pos <- select_positions(.data, ...)
  old_names <- colnames(.data)[col_pos]
  new_names_zero <- nchar(new_names) == 0L
  if (any(new_names_zero)) {
    warning("You didn't provide new names for: ", paste0("`", old_names[new_names_zero], collapse = ", "), "`")
    new_names[new_names_zero] <- old_names[new_names_zero]
  }
  colnames(.data)[col_pos] <- new_names
  .data
}

#' @param .fn A `function()` used to transform the selected `.cols`. Should return a character vector the same length as
#' the input.
#' @param .cols Columns to rename; defaults to all columns.
#'
#' @examples
#' rename_with(mtcars, toupper)
#' rename_with(mtcars, toupper, starts_with("c"))
#'
#' @rdname rename
#' @export
rename_with <- function(.data, .fn, .cols = everything(), ...) {
  UseMethod("rename_with")
}

#' @export
rename_with.data.frame <- function(.data, .fn, .cols = everything(), ...) {
  if (!is.function(.fn)) stop("`", .fn, "` is not a valid function")
  grouped <- is.grouped_df(.data)
  if (grouped) grp_pos <- which(colnames(.data) %in% group_vars(.data))
  col_pos <- eval_select_pos(.data = .data, .group_pos = TRUE, .cols = substitute(.cols))
  cols <- colnames(.data)[col_pos]
  new_cols <- .fn(cols, ...)
  if (any(duplicated(new_cols))) {
    stop("New names must be unique however `", deparse(substitute(.fn)), "` returns duplicate column names")
  }
  colnames(.data)[col_pos] <- new_cols
  if (grouped) .data <- groups_set(.data, colnames(.data)[grp_pos])
  .data
}
