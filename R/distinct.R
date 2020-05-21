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
  check_is_dataframe(.data)
  UseMethod("distinct")
}

#' @export
distinct.default <- function(.data, ..., .keep_all = FALSE) {
  if (ncol(.data) == 0L) return(.data[1, ])
  cols <- deparse_dots(...)
  col_names <- names(cols)
  col_len <- length(cols)
  if (is.null(col_names) && col_len > 0L) names(cols) <- cols
  if (col_len == 0L) {
    res <- .data
  } else {
    res <- mutate(.data, ...)
    col_names <- names(cols)
    res <- if (!is.null(col_names)) {
      zero_names <- nchar(col_names) == 0L
      if (any(zero_names)) {
        names(cols)[zero_names] <- cols[zero_names]
        col_names <- names(cols)
      }
      suppressMessages(select(res, col_names))
    } else {
      suppressMessages(select(res, cols))
    }
  }
  res <- unique(res)
  if (isTRUE(.keep_all)) {
    res <- cbind(res, .data[rownames(res), setdiff(colnames(.data), colnames(res)), drop = FALSE])
  }
  common_cols <- c(intersect(colnames(.data), colnames(res)), setdiff(col_names, colnames(.data)))
  if (length(common_cols) > 0L) res[, common_cols, drop = FALSE] else res
}

#' @export
distinct.grouped_data <- function(.data, ..., .keep_all = FALSE) {
  apply_grouped_function("distinct", .data, ..., .keep_all = .keep_all)
}
