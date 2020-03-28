#' Get integer column positions
#'
#' Given nse column names, get the integer column positions within the data.frame.
#'
#' @inheritParams select
#'
#' @return
#' A `vector` of `integer`s.
#'
#' @noRd
select_positions <- function(.data, ...) {
  # We need to remove the additional quotes when passed a string column name
  cols <- unname(gsub('[\"]', '', deparse_dots(...)))
  col_pos <- suppressWarnings(as.integer(cols))
  col_pos[is.na(col_pos)] <- match(cols[which(is.na(col_pos))], colnames(.data))
  col_pos
}
