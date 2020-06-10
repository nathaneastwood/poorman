deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}

deparse_var <- function(var, frame = if (is.null(eval_env$env)) parent.frame() else eval_env$env) {
  sub_var <- eval(substitute(substitute(var)), frame)
  if (is.symbol(sub_var)) var <- as.character(sub_var)
  var
}

dots_to_list <- function(...) {
  eval(substitute(alist(...)))
}

check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

is_wholenumber <- function(x) {
  x %% 1L == 0L
}

seq2 <- function (from, to) {
  if (length(from) != 1) stop("`from` must be length one")
  if (length(to) != 1) stop("`to` must be length one")
  if (from > to) integer() else seq.int(from, to)
}

is_function <- function(x, frame) {
  res <- tryCatch(
    is.function(x),
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  if (isTRUE(res)) return(res)
  res <- tryCatch(
    is.function(eval(x)),
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  if (isTRUE(res)) return(res)
  res <- tryCatch(
    is.function(eval(as.symbol(deparse(substitute(x))))),
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  if (isTRUE(res)) return(res)
  FALSE
}

collapse_to_sentence <- function(x) {
  len_x <- length(x)
  if (len_x == 0L) {
    stop("Length of `x` is 0")
  } else if (len_x == 1L) {
    as.character(x)
  } else if (len_x == 2L) {
    paste(x, collapse = " and ")
  } else {
    paste(paste(x[1:(len_x - 1)], collapse = ", "), x[len_x], sep = " and ")
  }
}
