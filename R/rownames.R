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
  if (var %in% colnames(.data)) stop("Column `", var, "` already exists in `.data`")
  row_names <- rownames(.data)
  rownames(.data) <- NULL
  .data <- inset(.data, , var, row_names)
  extract(.data, , c(var, setdiff(colnames(.data), var)))
}
