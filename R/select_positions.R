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
select_positions <- function(.data, ..., .group_pos = FALSE) {
  cols <- dotdotdot(...)
  select_env$setup(.data = .data, calling_frame = parent.frame(2L))
  on.exit(select_env$clean(), add = TRUE)
  data_names <- select_env$get_colnames()
  pos <- unlist(lapply(cols, eval_expr))
  col_len <- select_env$get_ncol()
  if (any(pos > col_len)) {
    oor <- pos[which(pos > col_len)]
    oor_len <- length(oor)
    stop(
      "Location", if (oor_len > 1) "s " else " ", collapse_to_sentence(oor),
      if (oor_len > 1) " don't " else " doesn't ", "exist. There are only ", col_len, " columns."
    )
  }
  if (isTRUE(.group_pos)) {
    groups <- get_groups(.data)
    missing_groups <- !(groups %in% cols)
    if (any(missing_groups)) {
      sel_missing <- groups[missing_groups]
      readd <- match(sel_missing, data_names)
      readd <- readd[!(readd %in% pos)]
      if (length(readd) > 0L) {
        message("Adding missing grouping variables: `", paste(sel_missing, collapse = "`, `"), "`")
        if (length(names(cols)) > 0L) names(readd) <- data_names[readd]
        pos <- c(readd, pos)
      }
    }
  }
  pos[!duplicated(pos)]
}

eval_expr <- function(x) {
  type <- typeof(x)
  switch(
    type,
    "integer" = x,
    "double" = as.integer(x),
    "character" = select_char(x),
    "symbol" = select_symbol(x),
    "language" = eval_call(x),
    stop("Expressions of type <", typeof(x), "> cannot be evaluated for use when subsetting.")
  )
}

select_char <- function(expr) {
  pos <- match(expr, select_env$get_colnames())
  if (is.na(pos)) stop("Column `", expr, "` does not exist")
  pos
}

select_symbol <- function(expr) {
  expr_name <- as.character(expr)
  if (grepl("^is\\.", expr_name) && is.function(expr)) {
    stop(
      "Predicate functions must be wrapped in `where()`.\n\n",
      sprintf("  data %%>%% select(where(%s))", expr_name)
    )
  }
  res <- try(select_char(as.character(expr)), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- tryCatch(
      unlist(lapply(eval(expr, envir = select_env$calling_frame), eval_expr)),
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
  eval(expr, envir = select_env$.data)
}

# -- Environment ---------------------------------------------------------------

select_env <- new.env()
select_env$setup <- function(.data, calling_frame) {
  select_env$.data <- .data
  select_env$calling_frame <- calling_frame
}
select_env$clean <- function() {
  rm(list = c(".data", "calling_frame"), envir = select_env)
}
select_env$get_colnames <- function() colnames(select_env$.data)
select_env$get_nrow <- function() nrow(select_env$.data)
select_env$get_ncol <- function() ncol(select_env$.data)

# -- Helpers -------------------------------------------------------------------

#' A cleaner interface to evaluating select_positions when column names are not passed via ...
#' @noRd
eval_select_pos <- function(.data, .cols, .group_pos = FALSE) {
  do.call(select_positions, list(.data = .data, .cols, .group_pos = .group_pos))
}
