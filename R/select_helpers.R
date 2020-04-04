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
  cols <- deparse_dots(...)
  cols <- unlist(lapply(
    cols,
    function(x) {
      parsed_x <- try(eval(parse(text = x)), silent = TRUE)
      if (inherits(parsed_x, "try-error")) parsed_x <- unname(gsub('[\"]', '', x))
      parsed_x
    }
  ))
  col_pos <- suppressWarnings(as.integer(cols))
  col_pos[is.na(col_pos)] <- match(cols[which(is.na(col_pos))], colnames(.data))
  unique(col_pos)
}

#' Select Helpers
#'
#' These functions allow you to select variables based on their names.
#' * `starts_with()`: Starts with a prefix.
#' * `ends_with()`: Ends with a prefix.
#'
#' @param match `character(n)`. If length > 1, the union of the matches is taken.
#' @param ignore.case `logical(1)`. If `TRUE`, the default, ignores case when matching names.
#' @param vars `character(n)`. A character vector of variable names.
#'
#' @return
#' An integer vector giving the position of the matched variables.
#'
#' @examples
#' mtcars %>% select(starts_with("c"))
#' mtcars %>% select(starts_with(c("c", "h")))
#'
#' @name select_helpers
#'
#' @export
starts_with <- function(match, ignore.case = TRUE, vars = colnames(get(".data", envir = parent.frame()))) {
  grep(paste0("^", paste0(match, collapse = "|^")), vars, ignore.case = ignore.case)
}

#' @name select_helpers
#' @export
ends_with <- function(match, ignore.case = TRUE, vars = colnames(get(".data", envir = parent.frame()))) {
  grep(paste0(paste0(match, collapse = "$|"), "$"), vars, ignore.case = ignore.case)
}
