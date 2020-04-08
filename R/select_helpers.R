#' Get integer column positions
#'
#' Given a set of column names, or column selection helper functions, evaluate and get the integer column positions
#' within the data.frame.
#'
#' @inheritParams select
#' @param group_pos `logical(1)`. Should grouping variable positions be returned (default: `FALSE`)?
#'
#' @return
#' A vector of `integer`s.
#'
#' @noRd
select_positions <- function(.data, ..., group_pos = FALSE) {
  # We need to remove the additional quotes when passed a string column name
  cols <- deparse_dots(...)
  data_names <- colnames(.data)
  cols <- unlist(lapply(
    cols,
    function(x) if (x %in% data_names) x else eval(parse(text = x))
  ))
  if (isTRUE(group_pos)) {
    groups <- group_vars(.data)
    missing_groups <- !(groups %in% cols)
    if (any(missing_groups)) {
      message("Adding missing grouping variables: `", paste(groups[missing_groups], collapse = "`, `"), "`")
      cols <- c(groups[missing_groups], cols)
    }
  }
  col_pos <- suppressWarnings(as.integer(cols))
  col_pos[is.na(col_pos)] <- match(cols[which(is.na(col_pos))], data_names)
  unique(col_pos)
}

#' Select Helpers
#'
#' These functions allow you to select variables based on their names.
#' * `starts_with()`: Starts with a prefix.
#' * `ends_with()`: Ends with a prefix.
#' * `contains()`: Contains a literal string.
#' * `matches()`: Matches a regular expression.
#' * `all_of()`: Matches variable names in a character vector. All names must be present, otherwise an error is thrown.
#' * `any_of()`: The same as `all_of()` except it doesn't throw an error.
#' * `everything()`: Matches all variables.
#' * `last_col()`: Select the last variable, possibly with an offset.
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
#' iris %>% select(matches(".t."))
#' mtcars %>% select(last_col())
#'
#' # `all_of()` selects the variables in a character vector:
#' iris %>% select(all_of(c("Petal.Length", "Petal.Width")))
#' # `all_of()` is strict and will throw an error if the column name isn't found
#' try({iris %>% select(all_of(c("Species", "Genres")))})
#' # However `any_of()` allows missing variables
#' iris %>% select(any_of(c("Species", "Genres")))
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

#' @param perl `logical(1)`. Should Perl-compatible regexps be used?
#' @name select_helpers
#' @export
matches <- function(match, ignore.case = TRUE, perl = FALSE, vars = colnames(get(".data", envir = parent.frame()))) {
  grep(pattern = match, x = vars, ignore.case = ignore.case, perl = perl)
}

#' @param x `character(n)`. A vector of column names.
#' @name select_helpers
#' @export
all_of <- function(x, vars = colnames(get(".data", envir = parent.frame()))) {
  x_ <- !x %in% vars
  if (any(x_)) {
    which_x_ <- which(x_)
    if (length(which_x_) == 1L) {
      stop("The column ", x[which_x_], " does not exist.")
    } else {
      stop("The columns ", paste(x[which_x_], collapse = ", "), " do not exist.")
    }
  } else {
    which(vars %in% x)
  }
}

#' @name select_helpers
#' @export
any_of <- function(x, vars = colnames(get(".data", envir = parent.frame()))) {
  which(vars %in% x)
}

#' @name select_helpers
#' @export
everything <- function(vars = colnames(get(".data", envir = parent.frame()))) {
  seq_along(vars)
}

#' @param offset `integer(1)`. Select the `n`th variable from the end of the `data.frame`.
#' @name select_helpers
#' @export
last_col <- function(offset = 0L, vars = colnames(get(".data", envir = parent.frame()))) {
  if (!is_wholenumber(offset)) stop("`offset` must be an integer")
  n <- length(vars)
  if (offset && n <= offset) {
    stop("`offset` must be smaller than the number of `vars`")
  } else if (n == 0) {
    stop("Can't select last column when `vars` is empty")
  } else {
    n - offset
  }
}
