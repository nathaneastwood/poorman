#' Arrange rows by variables
#'
#' Order rows of a `data.frame` by an expression involving its variables.
#'
#' @param .data A `data.frame`.
#' @param ... A comma separated vector of unquoted name(s) to order the data by.
#'
#' @examples
#' arrange(mtcars, mpg)
#' mtcars %>% arrange(mpg)
#' mtcars %>% arrange(cyl, mpg)
#'
#' @return
#' A `data.frame`.
#'
#' @export
arrange <- function(.data, ...) {
  UseMethod("arrange")
}

#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  dots <- dotdotdot(...)
  is_grouped <- has_groups(.data)
  if (isTRUE(.by_group)) dots <- c(groups(.data), dots)
  rows <- arrange_rows(.data = .data, dots)
  out <- .data[rows, , drop = FALSE]
  if (is_grouped) {
    attr(out, "groups") <- calculate_groups(out, group_vars(out))
  }
  out
}

## -- Helpers ------------------------------------------------------------------

arrange_rows <- function(.data, dots) {

  if (length(dots) == 0L) return(seq_len(nrow(.data)))

  directions <- vapply(
    dots,
    function(x) if (is.call(x) && deparse(x[[1]]) == "desc") "desc" else "asc",
    NA_character_
  )

  dots <- lapply(
    dots,
    function(x) if (is.call(x) && deparse(x[[1]]) == "desc") x[[2]] else x
  )

  data <- do.call(transmute, c(list(.data = ungroup(.data)), dots))
  descs <- which(directions == "desc")
  data[, colnames(data)[descs]] <- data[, colnames(data)[descs], drop = FALSE] * -1
  do.call(order, c(data, list(decreasing = FALSE, na.last = TRUE)))
}
