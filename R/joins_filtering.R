#' Filtering joins filter rows from `x` based on the presence or absence of
#' matches in `y`:
#'
#' * `semi_join()` return all rows from `x` with a match in `y`.
#' * `anti_join()` return all rows from `x` with*out* a match in `y`.
#'
#' @name filter_joins
NULL

#' @rdname filter_joins
#' @export
anti_join <- function(x, y, by = NULL) {
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
  }
  in_x_not_y <- !(interaction(extract(x, , by)) %in% interaction(extract(y, , by)))
  res <- extract(x, in_x_not_y, )
  rownames(res) <- NULL
  res
}

#' @rdname filter_joins
#' @export
semi_join <- function(x, y, by = NULL) {
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
  }
  in_x_and_y <- interaction(extract(x, , by)) %in% interaction(extract(y, , by))
  res <- extract(x, in_x_and_y, )
  rownames(res) <- NULL
  res
}
