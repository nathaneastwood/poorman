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
  row_number <- attr(.data, "row.names") # row.names returns a character vector
  out <- .data[rows, , drop = FALSE]
  if (is.numeric(row_number)) {
    row.names(out) <- row_number
  }
  if (is_grouped) {
    attr(out, "groups") <- calculate_groups(out, group_vars(out))
  }
  out
}

## -- Helpers ------------------------------------------------------------------

arrange_rows <- function(.data, dots) {

  if (length(dots) == 0L) return(seq_len(nrow(.data)))

  for (i in seq_along(dots)) {
    tmp <- deparse(dots[[i]])
    if (startsWith(tmp, "desc(")) {
      tmp <- gsub("^desc\\(", "-", tmp)
      tmp <- gsub("\\)$", "", tmp)
    }
    dots[[i]] <- parse(text = tmp, keep.source = FALSE)[[1]]
  }

  used <- unname(do.call(c, lapply(dots, find_used)))
  used <- used[used %in% colnames(.data)]
  for (i in seq_along(dots)) {
    if (is.character(.data[[used[[i]]]])) {
      .data[[used[[i]]]] <- factor(.data[[used[[i]]]])
    }
    if (is.factor(.data[[used[[i]]]]) &&
        (startsWith(deparse(dots[[i]]), "desc(") ||
         startsWith(deparse(dots[[i]]), "-"))) {
      dots[[i]] <- bquote(-xtfrm(.(as.name(used[[i]]))))
    }
  }

  data <- do.call(transmute, c(list(.data = ungroup(.data)), dots))
  do.call(order, c(data, list(decreasing = FALSE, na.last = TRUE)))

}
