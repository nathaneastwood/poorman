#' Filtering joins filter rows from `x` based on the presence or absence of
#' matches in `y`:
#'
#' * `semi_join()` return all rows from `x` with a match in `y`.
#' * `anti_join()` return all rows from `x` with*out* a match in `y`.
#'
#' @param x,y The `data.frame`s to join.
#' @param by A character vector of variables to join by. If `NULL`, the default,
#' `*_join()` will do a natural join, using all variables with common names
#' across the two tables. A message lists the variables so that you can check
#' they're right (to suppress the message, simply explicitly list the variables
#' that you want to join).
#'
#' @examples
#' table1 <- data.frame(
#'   pupil = rep(1:3, each = 2),
#'   test = rep(c("A", "B"), 3),
#'   score = c(60, 70, 65, 80, 85, 70),
#'   stringsAsFactors = FALSE
#' )
#' table2 <- table1[c(1, 3, 4), ]
#'
#' table1 %>% anti_join(table2, by = c("pupil", "test"))
#' table1 %>% semi_join(table2, by = c("pupil", "test"))
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
  rows <- interaction(x[, by]) %in% interaction(y[, by])
  if (type == "anti") rows <- !rows
  res <- x[rows, ]
  rownames(res) <- NULL
  res
}
