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
  filter_join_worker(x, y, by, type = "anti")
}

#' @rdname filter_joins
#' @export
semi_join <- function(x, y, by = NULL) {
  filter_join_worker(x, y, by, type = "semi")
}

filter_join_worker <- function(x, y, by = NULL, type = c("anti", "semi")) {
  type <- match.arg(type, choices = c("anti", "semi"), several.ok = FALSE)
  if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
  }
  rows <- interaction(extract(x, , by)) %in% interaction(extract(y, , by))
  if (type == "anti") rows <- !rows
  res <- extract(x, rows, )
  rownames(res) <- NULL
  res
}
