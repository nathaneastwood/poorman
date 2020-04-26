deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

deparse_var <- function(var) {
  sub_var <- eval(substitute(substitute(var)), parent.frame())
  if (is.symbol(sub_var)) var <- as.character(sub_var)
  var
}

check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

is_wholenumber <- function(x) {
  x %% 1L == 0L
}

set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}
