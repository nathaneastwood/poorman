#' @param x A `data.frame`.
#' @return
#' When using [ungroup()], a `data.frame`.
#' @rdname group_by
#' @export
ungroup <- function(x, ...) {
  UseMethod("ungroup")
}

#' @export
ungroup.data.frame <- function(x, ...) {
  rm_groups <- deparse_dots(...)
  groups <- group_vars(x)
  if (length(rm_groups) == 0L) rm_groups <- groups
  x <- groups_set(x, groups[!(groups %in% rm_groups)])
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- class(x)[!(class(x) %in% "grouped_data")]
  }
  x
}

#' @export
ungroup.grouped_data <- function(x, ...) {
  NextMethod("ungroup")
}
