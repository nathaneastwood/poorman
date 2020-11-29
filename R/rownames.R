#' Tools for working with row names
#'
#' In some quarters, it is considered best to avoid row names, because they are effectively a character column with
#' different semantics than every other column.
#' These functions allow to you detect if a `data.frame` has row names (`has_rownames()`), remove them
#' (`remove_rownames()`), or convert them back-and-forth between an explicit column (`rownames_to_column()` and
#' `column_to_rownames()`). Also included is `rowid_to_column()`, which adds a column at the start of the dataframe of
#' ascending sequential row ids starting at 1. Note that this will remove any existing row names.
#'
#' @param .data A `data.frame`.
#' @param var `character(1)`. The name of the column to use for row names.
#'
#' @returns
#' * `column_to_rownames()` always returns a `data.frame`.
#' * `has_rownames()` returns a `logical(1)`.
#' * All other functions return an object of the same class as the input.
#'
#' @examples
#' # Detect row names
#' has_rownames(mtcars)
#' has_rownames(iris)
#'
#' # Remove row names
#' remove_rownames(mtcars) %>% has_rownames()
#'
#' # Convert between row names and column
#' mtcars <- rownames_to_column(mtcars, var = "car")
#' column_to_rownames(mtcars, var = "car") %>% head()
#'
#' # Adding rowid as a column
#' rowid_to_column(iris) %>% head()
#'
#' @name rownames
#' @export
rownames_to_column <- function(.data, var = "rowname") {
  check_is_dataframe(.data)
  col_names <- colnames(.data)
  if (var %in% col_names) stop("Column `", var, "` already exists in `.data`.")
  .data[, var] <- rownames(.data)
  rownames(.data) <- NULL
  .data[, c(var, setdiff(col_names, var))]
}

#' @rdname rownames
#' @export
rowid_to_column <- function(.data, var = "rowid") {
  check_is_dataframe(.data)
  if (var %in% colnames(.data)) stop("Column `", var, "` already exists in `.data`.")
  args <- list(.data = .data, quote(row_number()), .before = 1L)
  names(args)[2L] <- var
  remove_rownames(do.call(mutate, args))
}

#' @rdname rownames
#' @export
column_to_rownames <- function(.data, var = "rowname") {
  check_is_dataframe(.data)
  if (has_rownames(.data)) stop("`.data` already has rownames")
  if (!(var %in% colnames(.data))) stop("Column `", var, "` could not be found in `.data`.")
  rownames(.data) <- .data[[var]]
  .data[[var]] <- NULL
  .data
}

#' @rdname rownames
#' @export
remove_rownames <- function(.data) {
  check_is_dataframe(.data)
  rownames(.data) <- NULL
  .data
}

#' @rdname rownames
#' @export
has_rownames <- function(.data) {
  .row_names_info(.data) > 0L
}
