#' Arrange rows by variables
#'
#' Order rows of a `data.frame` by an expression involving its variables.
#'
#' @param .data A `data.frame`.
#' @param ... A comma separated vector of unquoted name(s) or expressions to order the data by. Use [desc()] to sort a
#' variable in descending order. Use [across()] to sort by a selection of columns.
#'
#' @examples
#' arrange(mtcars, mpg)
#' mtcars %>% arrange(mpg)
#' mtcars %>% arrange(cyl, mpg)
#' mtcars %>% arrange(desc(mpg))
#' mtcars %>% arrange(across(starts_with("c")))
#' mtcars %>% arrange(across(starts_with("c"), desc))
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
  rows <- arrange_rows(.data = .data, dots = dots, env = parent.frame())
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

arrange_rows <- function(.data, dots, env = parent.frame()) {

  if (length(dots) == 0L) return(seq_len(nrow(.data)))

  # A top level desc() sets the direction rather than being evaluated. A top level unary minus is treated in
  # the same way so that it can be used with non-numeric columns (#99).
  descending <- vapply(dots, is_desc_call, NA)
  dots[descending] <- lapply(dots[descending], `[[`, 2L)

  .data <- ungroup(.data)
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  mask <- new.env(parent = env)
  mask$.data <- .data

  n <- nrow(.data)
  keys <- vector(mode = "list", length = length(dots))
  for (i in seq_along(dots)) {
    res <- eval(dots[[i]], envir = context$as_env(), enclos = mask)
    keys[[i]] <- arrange_keys(res, descending[[i]], n, dots[[i]])
  }
  keys <- unlist(keys, recursive = FALSE, use.names = FALSE)
  if (length(keys) == 0L) return(seq_len(n))
  do.call(order, c(keys, list(decreasing = FALSE, na.last = TRUE)))

}

is_desc_call <- function(expr) {
  if (!is.call(expr) || length(expr) != 2L) return(FALSE)
  fn <- expr[[1L]]
  if (is.call(fn) && (identical(fn[[1L]], as.name("::")) || identical(fn[[1L]], as.name(":::")))) fn <- fn[[3L]]
  identical(fn, as.name("desc")) || identical(fn, as.name("-"))
}

# Convert the result of an arrange() expression into a list of sort keys for order(). Data frames, such as the output
# of across(), and matrices are split into one key per column.
arrange_keys <- function(x, descending, n, expr) {
  if (is.data.frame(x) || is.matrix(x)) {
    cols <- if (is.data.frame(x)) unname(as.list(x)) else lapply(seq_len(ncol(x)), function(j) x[, j])
    return(unlist(lapply(cols, arrange_keys, descending, n, expr), recursive = FALSE))
  }
  size <- NROW(x)
  if (size != n) {
    if (size != 1L) {
      stop(sprintf("`%s` must be size %d or 1, not %d.", paste(deparse(expr), collapse = " "), n, size))
    }
    x <- rep(x, length.out = n)
  }
  if (is.list(x) && !inherits(x, "POSIXlt")) {
    x <- match(x, unique(x))
  } else if (is.character(x)) {
    # Sorting strings in the current locale is slow, so only rank the unique values and sort by their integer rank.
    uniq <- unique(x)
    x <- rank(uniq, na.last = "keep", ties.method = "min")[match(x, uniq)]
  }
  if (descending) x <- -xtfrm(x)
  list(x)
}
