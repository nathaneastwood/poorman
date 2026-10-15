check_is_dataframe <- function(.data) {
  parent_fn <- all.names(sys.call(-1L), max.names = 1L)
  if (!is.data.frame(.data)) stop(parent_fn, " must be given a data.frame")
  invisible()
}

seq2 <- function(from, to) {
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
    structure(list(x = x), class = "data.frame", row.names = c(NA, -1L))
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

#' Remove all levels of a list
#' @noRd
squash <- function(lst) {
  do.call(c, lapply(lst, function(x) if (is.list(x) && !is.data.frame(x)) squash(x) else list(x)))
}

#' Move entries within a list up one level
#' @noRd
flatten <- function(lst) {
  nested <- is_nested(lst)
  res <- c(lst[!nested], unlist(lst[nested], recursive = FALSE))
  if (sum(nested)) Recall(res) else return(res)
}

#' Subset a vector, keeping its attributes
#'
#' Unlike `[`, this keeps all of the attributes of `x` (e.g. custom classes and attributes), except names.
#'
#' @param x A vector, `matrix` or `data.frame`.
#' @param i `integer()`. The positions to keep. `NA` positions return missing values.
#'
#' @noRd
vec_slice <- function(x, i) {
  if (!is.null(dim(x))) return(x[i, , drop = FALSE])
  res <- x[i]
  attrs <- attributes(x)
  if (!is.null(attrs) && is.atomic(x)) {
    attrs$names <- NULL
    attributes(res) <- attrs
  }
  res
}

#' Identify the unique rows of a set of columns
#'
#' @param x A `list` of equal length vectors.
#' @param n `integer(1)`. The length of the vectors, used when `x` is empty.
#'
#' @return An `integer` vector giving, for each row, the position of that row's first appearance amongst the unique
#' rows. Missing values are treated as equal to one another.
#'
#' @noRd
vec_group_id <- function(x, n = length(x[[1L]])) {
  if (length(x) == 0L) return(rep(1L, n))
  id <- NULL
  for (col in x) {
    if (is.factor(col) || inherits(col, c("Date", "POSIXct"))) col <- unclass(col)
    col_id <- match(col, unique(col))
    if (is.null(id)) {
      id <- col_id
    } else {
      id <- (id - 1) * max(col_id, 0L) + col_id
      id <- match(id, unique(id))
    }
  }
  id
}
