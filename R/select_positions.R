#' Get integer column positions
#'
#' Within the context of a selection function (`select`, `relocate`, `rename`), return the integer position of the
#' columns selected.
#'
#' @details
#' This function accepts
#'
#' * `integer()`s
#' * `numeric()`s
#' * `character()`s
#' * `symbol()`s
#' * `call()`s
#'
#' Each type is handled separately.
#'
#' @inheritParams select
#' @param group_pos `logical(1)`. Should grouping variable positions be returned (default: `FALSE`)?
#'
#' @return
#' A vector of `integer`s.
#'
#' @examples
#' select_positions(mtcars, mpg)
#' select_positions(mtcars, "mpg")
#' select_positions(mtcars, starts_with("m"))
#' select_positions(mtcars, -mpg)
#' select_positions(mtcars, mpg:drat)
#'
#' @noRd
select_positions <- function(.data, ..., group_pos = FALSE) {
  cols <- eval(substitute(alist(...)))
  data_names <- colnames(.data)
  select_env$.col_names <- data_names
  on.exit(rm(list = ".col_names", envir = select_env))
  exec_env <- parent.frame(2L)
  pos <- unlist(lapply(cols, eval_expr, exec_env = exec_env))
  if (isTRUE(group_pos)) {
    groups <- get_groups(.data)
    missing_groups <- !(groups %in% cols)
    if (any(missing_groups)) {
      sel_missing <- groups[missing_groups]
      message("Adding missing grouping variables: `", paste(sel_missing, collapse = "`, `"), "`")
      readd <- match(sel_missing, data_names)
      if (length(names(cols)) > 0L) names(readd) <- data_names[readd]
      pos <- c(readd, pos)
    }
  }
  pos[!duplicated(pos)]
}

eval_expr <- function(x, exec_env) {
  type <- typeof(x)
  switch(
    type,
    "integer" = x,
    "double" = as.integer(x),
    "character" = select_char(x),
    "symbol" = select_symbol(x, exec_env = exec_env),
    "language" = eval_call(x),
    stop("Expressions of type <", typeof(x), "> cannot be evaluated for use when subsetting.")
  )
}

select_char <- function(expr) {
  pos <- match(expr, select_env$.col_names)
  if (is.na(pos)) stop("Column `", expr, "` does not exist")
  pos
}

select_symbol <- function(expr, exec_env) {
  res <- try(select_char(as.character(expr)), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- tryCatch(
      eval_expr(eval(expr, envir = exec_env)),
      error = function(e) stop("Column ", expr, " does not exist.")
    )
  }
  res
}

eval_call <- function(x) {
  type <- as.character(x[[1]])
  switch(
    type,
    `:` = select_seq(x),
    `!` = select_negate(x),
    `-` = select_minus(x),
    `c` = select_c(x),
    `(` = select_bracket(x),
    select_context(x)
  )
}

select_seq <- function(expr) {
  x <- eval_expr(expr[[2]])
  y <- eval_expr(expr[[3]])
  x:y
}

select_negate <- function(expr) {
  x <- if (is_negated_colon(expr)) {
    expr <- call(":", expr[[2]][[2]], expr[[2]][[3]][[2]])
    eval_expr(expr)
  } else {
    eval_expr(expr[[2]])
  }
  x * -1L
}

is_negated_colon <- function(expr) {
  expr[[1]] == "!" && length(expr[[2]]) > 1L && expr[[2]][[1]] == ":" && expr[[2]][[3]][[1]] == "!"
}

select_minus <- function(expr) {
  x <- eval_expr(expr[[2]])
  x * -1L
}

select_c <- function(expr) {
  lst_expr <- as.list(expr)
  lst_expr[[1]] <- NULL
  unlist(lapply(lst_expr, eval_expr))
}

select_bracket <- function(expr) {
  eval_expr(expr[[2]])
}

select_context <- function(expr) {
  eval(expr, envir = context$.data)
}
