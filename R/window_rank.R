#' Windowed Rank Functions
#'
#' Six variations on ranking functions, mimicking the ranking functions described in SQL2003. They are currently
#' implemented using the built in [rank()] function. All ranking functions map smallest inputs to smallest outputs. Use
#' `desc()` to reverse the direction.
#'
#' @details
#' * `cume_dist()`: a cumulative distribution function. Proportion of all values less than or equal to the current rank.
#' * `dense_rank()`: like `min_rank()`, but with no gaps between ranks
#' * `min_rank()`: equivalent to `rank(ties.method = "min")`
#' * `ntile()`: a rough rank, which breaks the input vector into `n` buckets. The size of the buckets may differ by up
#'    to one, larger buckets have lower rank.
#' * `percent_rank()`: a number between `0` and `1` computed by rescaling `min_rank` to `[0, 1]`
#' * `row_number()`: equivalent to `rank(ties.method = "first")`
#'
#' @param x A vector of values to rank. Missing values are left as is. If you want to treat them as the smallest or
#' largest values, replace with `Inf` or `-Inf` before ranking.
#' @param n `integer(1)`. The number of groups to split up into.
#'
#' @examples
#' x <- c(5, 1, 3, 2, 2, NA)
#' row_number(x)
#' min_rank(x)
#' dense_rank(x)
#' percent_rank(x)
#' cume_dist(x)
#'
#' ntile(x, 2)
#' ntile(1:8, 3)
#'
#' # row_number can be used with single table verbs without specifying x
#' # (for data frames and databases that support windowing)
#' mutate(mtcars, row_number() == 1L)
#' mtcars %>% filter(between(row_number(), 1, 10))
#'
#' @name window_rank
NULL

#' @rdname window_rank
#' @export
cume_dist <- function(x) {
  rank(x, ties.method = "max", na.last = "keep") / sum(!is.na(x))
}

#' @rdname window_rank
#' @export
dense_rank <- function(x) {
  match(x, sort(unique(x)))
}

#' @rdname window_rank
#' @export
min_rank <- function(x) {
  rank(x, ties.method = "min", na.last = "keep")
}

#' @rdname window_rank
#' @export
ntile <- function (x = row_number(), n) {
  if (!missing(x)) x <- row_number(x)
  len <- length(x) - sum(is.na(x))
  n <- as.integer(floor(n))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  } else {
    n_larger <- as.integer(len %% n)
    n_smaller <- as.integer(n - n_larger)
    size <- len / n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- if_else(
      x <= larger_threshold,
      (x + (larger_size - 1L)) / larger_size,
      (x + (-larger_threshold + smaller_size - 1L)) / smaller_size + n_larger
    )
    as.integer(floor(bins))
  }
}

#' @rdname window_rank
#' @export
percent_rank <- function(x) {
  (min_rank(x) - 1) / (sum(!is.na(x)) - 1)
}

#' @rdname window_rank
#' @export
row_number <- function(x) {
  if (missing(x)) seq_len(n()) else rank(x, ties.method = "first", na.last = "keep")
}
