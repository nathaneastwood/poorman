#' Unite Multiple Columns Into One
#'
#' Convenience function to paste together multiple columns.
#'
#' @param data A `data.frame`.
#' @param col `character(1)` or `symbol(1)`. The name of the new column.
#' @param ... The columns to unite.
#' @param sep `character(1)`. Separator to use between the values.
#' @param remove `logical(1)`. If `TRUE`, remove the input columns from the output `data.frame`.
#' @param na.rm `logical(1)`. If `TRUE`, missing values will be remove prior to uniting each value.
#'
#' @return
#' A `data.frame` with the columns passed via `...` pasted together in a new column.
#'
#' @examples
#' df <- data.frame(x = c("a", "a", NA, NA), y = c("b", NA, "b", NA))
#' df
#'
#' df %>% unite("z", x:y, remove = FALSE)
#' # To remove missing values:
#' df %>% unite("z", x:y, na.rm = TRUE, remove = FALSE)
#'
#' @importFrom stats na.omit
#'
#' @export
unite <- function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  col <- deparse_var(col)
  cols_pos <- if (missing(...)) seq_along(data) else select_positions(data, ...)
  to_unite <- data[, cols_pos, drop = FALSE]
  data[[col]] <- if (isTRUE(na.rm)) {
    apply(to_unite, 1L, function(x) paste(stats::na.omit(x), collapse = sep))
  } else {
    apply(to_unite, 1L, paste, collapse = sep)
  }
  if (isTRUE(remove)) {
    to_rm <- setdiff(colnames(to_unite), col)
    if (inherits(data, "grouped_data") && length(to_rm) > 0L) {
      rm_groups <- as_symbols(intersect(group_vars(data), to_rm))
      data <- do.call(ungroup, squash(list(x = data, rm_groups)))
    }
    data[, to_rm] <- NULL
  } else {
    data <- eval(bquote(relocate(data, col, .before = .(cols_pos[1]))))
  }
  data
}
