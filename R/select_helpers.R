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
#' * `contains()`: Contains a literal string.
#'
#' @param match `character(n)`. If length > 1, the union of the matches is taken.
#' @param ignore.case `logical(1)`. If `TRUE`, the default, ignores case when matching names.
#' @param vars `character(n)`. A character vector of variable names. When called from inside selecting functions such as
#' `select()`, these are automatically set to the names of the table.
#'
#' @return
#' An integer vector giving the position of the matched variables.
#'
#' @examples
#' mtcars %>% select(starts_with("c"))
#' mtcars %>% select(starts_with(c("c", "h")))
#' mtcars %>% select(ends_with("b"))
#' mtcars %>% relocate(contains("a"), .before = mpg)
#'
#' @name select_helpers
#'
#' @export
starts_with <- function(match, ignore.case = TRUE, vars = colnames(get(".data", envir = parent.frame()))) {
  grep(pattern = paste0("^", paste0(match, collapse = "|^")), x = vars, ignore.case = ignore.case)
}

#' @name select_helpers
#' @export
ends_with <- function(match, ignore.case = TRUE, vars = colnames(get(".data", envir = parent.frame()))) {
  grep(pattern = paste0(paste0(match, collapse = "$|"), "$"), x = vars, ignore.case = ignore.case)
}

#' @name select_helpers
#' @export
contains <- function(match, ignore.case = TRUE, vars = colnames(get(".data", envir = parent.frame()))) {
  matches <- lapply(
    match,
    function(x) {
      if (isTRUE(ignore.case)) {
        match_u <- toupper(x)
        match_l <- tolower(x)
        pos_u <- grep(pattern = match_u, x = toupper(vars), fixed = TRUE)
        pos_l <- grep(pattern = match_l, x = tolower(vars), fixed = TRUE)
        unique(c(pos_l, pos_u))
      } else {
        grep(pattern = x, x = vars, fixed = TRUE)
      }
    }
  )
  unique(matches)
}
