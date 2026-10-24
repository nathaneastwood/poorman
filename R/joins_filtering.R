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
#' To join by different variables on x and y use a named vector. For example, `by = c("a" = "b")` will match `x.a` to
#' `y.b`.
#' @param copy Ignored. Included for compatibility with `dplyr`, where it allows `y` to come from a different data
#' source.
#' @param ... Ignored. Included for compatibility with `dplyr`.
#' @param na_matches Should `NA` and `NaN` values match one another?
#'
#' The default, `"na"`, treats two `NA` or two `NaN` values as equal, like `%in%` and [match()].
#'
#' Use `"never"` to always treat two `NA` or `NaN` values as different, so that a row of `x` with a missing value in
#' any of its keys never has a match in `y`.
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
#' sensors <- data.frame(site = c("north", NA, "south"), reading = c(4.2, 3.9, 5.1))
#' sites <- data.frame(site = c("north", NA))
#' semi_join(sensors, sites, by = "site")
#' semi_join(sensors, sites, by = "site", na_matches = "never")
#'
#' @name filter_joins
NULL

#' @rdname filter_joins
#' @export
anti_join <- function(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")) {
  filter_join_worker(x, y, by, na_matches, type = "anti")
}

#' @rdname filter_joins
#' @export
semi_join <- function(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")) {
  filter_join_worker(x, y, by, na_matches, type = "semi")
}

filter_join_worker <- function(x, y, by = NULL, na_matches = c("na", "never"), type = c("anti", "semi")) {
  type <- match.arg(type, choices = c("anti", "semi"), several.ok = FALSE)
  na_matches <- match.arg(arg = na_matches, choices = c("na", "never"), several.ok = FALSE)
  by <- join_key_names(by, names(x), names(y))
  keys <- join_keys(x, y, by, never_na = na_matches == "never")
  rows <- !is.na(keys$x_ids) & keys$x_ids %in% keys$y_ids
  if (type == "anti") rows <- !rows
  res <- x[rows, , drop = FALSE]
  rownames(res) <- NULL
  reconstruct_attrs(res, x)
}
