#' Tools for working with row names
#'
#' @param .data A `data.frame`.
#' @param var `character(1)`. The name of the column to use for row names.
#'
#' @returns A `data.frame`
#'
#' @examples
#' mtcars %>% rownames_to_column()
#'
#' @name rownames
#' @export
rownames_to_column <- function(.data, var = "rowname") {
  check_is_dataframe(.data)
  col_names <- colnames(.data)
  if (var %in% col_names) stop("Column `", var, "` already exists in `.data`")
  .data[, var] <- rownames(.data)
  rownames(.data) <- NULL
  .data[, c(var, setdiff(col_names, var))]
}
