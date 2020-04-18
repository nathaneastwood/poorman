#' Descending order
#'
#' Transform a vector into a format that will be sorted in descending order. This is useful within [arrange()].
#'
#' @param x A vector to transform.
#'
#' @examples
#' desc(1:10)
#' desc(factor(letters))
#'
#' first_day <- seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "years")
#' desc(first_day)
#'
#' mtcars %>% arrange(desc(mpg))
#'
#' @return
#' A vector of the same length as `x`.
#'
#' @export
desc <- function(x) -xtfrm(x)
