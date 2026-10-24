#' Subset distinct/unique rows
#'
#' Select only distinct/unique rows from a `data.frame`.
#'
#' @param .data A `data.frame`.
#' @param ... Optional variables to use when determining uniqueness. If there are multiple rows for a given combination
#' of inputs, only the first row will be preserved. If omitted, will use all variables.
#' @param .keep_all `logical(1)`. If `TRUE`, keep all variables in `.data`. If a combination of `...` is not distinct,
#' this keeps the first row of values.
#'
#' @examples
#' df <- data.frame(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#' nrow(df)
#' nrow(distinct(df))
#' nrow(distinct(df, x, y))
#'
#' distinct(df, x)
#' distinct(df, y)
#'
#' # You can choose to keep all other variables as well
#' distinct(df, x, .keep_all = TRUE)
#' distinct(df, y, .keep_all = TRUE)
#'
#' # You can also use distinct on computed variables
#' distinct(df, diff = abs(x - y))
#'
#' # The same behaviour applies for grouped data frames,
#' # except that the grouping variables are always included
#' df <- data.frame(
#'   g = c(1, 1, 2, 2),
#'   x = c(1, 1, 2, 1)
#' ) %>% group_by(g)
#' df %>% distinct(x)
#'
#' @return
#' A `data.frame` with the following properties:
#'
#' * Rows are a subset of the input but appear in the same order.
#' * Columns are not modified if `...` is empty or `.keep_all` is `TRUE`. Otherwise, `distinct()` first calls `mutate()`
#'   to create new columns.
#' * Groups are not modified.
#' * `data.frame` attributes are preserved.
#'
#' @export
distinct <- function(.data, ..., .keep_all = FALSE) {
  UseMethod("distinct")
}

#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  if (ncol(.data) == 0L) return(.data[1, ])
  dots <- dotdotdot(..., .env = parent.frame())
  mut <- mutate_df(.data, dots, parent.frame())
  data <- mut$data
  # As in dplyr, the grouping columns are always used; the columns are kept in the order of the data
  cols <- if (length(dots) == 0L) names(data) else intersect(names(data), c(group_vars(.data), mut$new_cols))
  keep <- which(!distinct_duplicated(.subset(data, cols), nrow(data)))
  if (!isTRUE(.keep_all)) data <- data[, cols, drop = FALSE]
  res <- subset_rows(data, keep)
  if (is.numeric(attr(.data, "row.names"))) attr(res, "row.names") <- .set_row_names(length(keep))
  if (has_groups(.data)) res <- groups_set(res, group_vars(.data), group_by_drop_default(.data))
  res
}

#' Find the rows which repeat an earlier row
#'
#' @param cols A `list` of the columns which decide whether rows are distinct.
#' @param n `integer(1)`. The number of rows.
#' @return `logical(n)`. `TRUE` for each row whose values in `cols` are those of an earlier row.
#' @noRd
distinct_duplicated <- function(cols, n) {
  # `group_ids()` is faster but only handles vectors whose values it can compare
  simple <- vapply(cols, function(col) is.atomic(col) && is.null(dim(col)), NA)
  if (all(simple)) return(duplicated(group_ids(cols, n)))
  duplicated(structure(cols, row.names = .set_row_names(n), class = "data.frame"))
}
