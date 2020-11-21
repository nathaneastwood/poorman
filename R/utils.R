check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

seq2 <- function (from, to) {
  if (length(from) != 1) stop("`from` must be length one")
  if (length(to) != 1) stop("`to` must be length one")
  if (from > to) integer() else seq.int(from, to)
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

#' Build a `data.frame` from a variety of inputs including atomic vectors, lists and other `data.frame`s
#' @noRd
build_data_frame <- function(x, nms = NULL) {
  res <- if (is.atomic(x)) {
    data.frame(x)
  } else if (is.list(x) && !is.data.frame(x)) {
    data.frame(I(x))
  } else if (is.data.frame(x)) {
    x
  }
  if (!is.null(nms)) colnames(res) <- nms
  res
}

#' Check whether any elements of a list are nested
#' @param lst A `list()`
#' @examples
#' is_nested(list(a = 1, b = 2, c = 3))
#' is_nested(list(a = 1, b = list(c = 2, d = 3)))
#' @noRd
is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)
