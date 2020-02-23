#' Group By
#'
#' Determine the groups within a `data.frame` to perform operations on.
#'
#' @param .data `data.frame`. The data to group.
#' @param ... One or more unquoted column names to group/ungroup the data by.
#'
#' @examples
#' group_by(mtcars, am, cyl)
#' ungroup(mutate(group_by(mtcars, am, cyl), sumMpg = sum(mpg)))
#' \dontrun{
#' mtcars %>%
#'   group_by(am, cyl) %>%
#'   mutate(sumMpg = sum(mpg)) %>%
#'   ungroup()
#'
#' mtcars %>%
#'   group_by(carb) %>%
#'   filter(any(gear == 5))
#' }
#'
#' @name groups
#' @export
group_by <- function(.data, ...) {
  groups <- deparse_dots(...)
  structure(.data, class = c("grouped_df", class(.data)), groups = groups)
}

#' @param x A `data.frame`.
#' @rdname groups
#' @export
ungroup <- function(x, ...) {
  rm_groups <- deparse_dots(...)
  groups <- attr(x, "groups")
  if (length(rm_groups) == 0L) rm_groups <- groups
  attr(x, "groups") <- groups[!(groups %in% rm_groups)]
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- class(x)[!(class(x) %in% "grouped_df")]
  }
  x
}

split_into_groups <- function(.data) {
  class(.data) <- "data.frame"
  groups <- attr(.data, "groups", exact = TRUE)
  groups <- lapply(
    groups,
    function(x, .data) as.factor(extract2(.data, x)),
    .data
  )
  res <- split(x = .data, f = groups)
  res
}

apply_grouped_function <- function(.data, fn, ...) {
  grouped <- split_into_groups(.data)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  class(res) <- c("grouped_df", class(res))
  res
}

#' @export
print.grouped_df <- function(x, ...) {
  class(x) <- "data.frame"
  print(x, ...)
  cat("\nGroups: ", paste(attr(x, "groups", exact = TRUE), collapse = ", "))
}
