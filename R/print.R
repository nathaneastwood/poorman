#' Print a grouped `data.frame`
#'
#' A print method for grouped `data.frame`s. Uses the standard `print.data.frame()` method but also reports the groups.
#'
#' @param x An object of class `grouped_df`.
#' @param ... Additional arguments to [print()].
#' @inheritParams base::print.data.frame
#'
#' @examples
#' mtcars %>% group_by(cyl, am) %>% print()
#'
#' @noRd
print.grouped_df <- function(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {
  class(x) <- "data.frame"
  print(x, ..., digits = digits, quote = quote, right = right, row.names = row.names, max = max)
  cat("\nGroups: ", paste(group_vars(x), collapse = ", "), "\n\n")
}
