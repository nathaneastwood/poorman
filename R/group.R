#' Group by one or more variables
#'
#' Determine the groups within a `data.frame` to perform operations on. [ungroup()] removes the grouping levels.
#'
#' @param .data `data.frame`. The data to group.
#' @param ... One or more unquoted column names to group/ungroup the data by.
#' @param .add `logical(1)`. When `FALSE` (the default) `group_by()` will override existing groups. To add to existing
#' groups, use `.add = TRUE`.
#'
#' @examples
#' group_by(mtcars, am, cyl)
#' ungroup(mutate(group_by(mtcars, am, cyl), sumMpg = sum(mpg)))
#' mtcars %>%
#'   group_by(am, cyl) %>%
#'   mutate(sumMpg = sum(mpg)) %>%
#'   ungroup()
#' mtcars %>%
#'   group_by(carb) %>%
#'   filter(any(gear == 5))
#'
#' @return
#' When using [group_by()], a `data.frame`, grouped by the grouping variables.
#'
#' @name groups
#' @export
group_by <- function(.data, ..., .add = FALSE) {
  check_is_dataframe(.data)
  pre_groups <- get_groups(.data)
  groups <- deparse_dots(...)
  if (isTRUE(.add)) groups <- unique(c(pre_groups, groups))
  unknown <- !(groups %in% colnames(.data))
  if (any(unknown)) stop("Invalid groups: ", groups[unknown])
  structure(.data, class = c("grouped_data", class(.data)), groups = groups)
}

#' @param x A `data.frame`.
#' @return
#' When using [ungroup()], `data.frame`.
#' @rdname groups
#' @export
ungroup <- function(x, ...) {
  check_is_dataframe(x)
  rm_groups <- deparse_dots(...)
  groups <- attr(x, "groups")
  if (length(rm_groups) == 0L) rm_groups <- groups
  attr(x, "groups") <- groups[!(groups %in% rm_groups)]
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- class(x)[!(class(x) %in% "grouped_data")]
  }
  x
}

get_groups <- function(x) {
  groups <- attr(x, "groups", exact = TRUE)
  if (is.null(groups)) character(0) else groups
}

has_groups <- function(x) {
  groups <- get_groups(x)
  if (length(groups) == 0L) FALSE else TRUE
}

set_groups <- function(x, groups) {
  attr(x, "groups") <- groups
  x
}

#' @param fn `character(1)`. The function to apply to each group.
#' @param .data A `data.frame`.
#' @param ... Arguments to be passed to `fn`.
#' @noRd
apply_grouped_function <- function(fn, .data, ...) {
  groups <- get_groups(.data)
  grouped <- split_into_groups(.data, groups)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  if (any(groups %in% colnames(res))) {
    class(res) <- c("grouped_data", class(res))
    attr(res, "groups") <- groups[groups %in% colnames(res)]
  }
  res
}

#' Print a grouped `data.frame`
#'
#' A print method for grouped `data.frame`s. Uses the standard `print.data.frame()` method but also reports the groups.
#'
#' @param x An object of class `grouped_data`.
#' @param ... Additional arguments to [print()].
#' @inheritParams base::print.data.frame
#'
#' @examples
#' mtcars %>% group_by(cyl, am) %>% print()
#'
#' @export
print.grouped_data <- function(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {
  class(x) <- "data.frame"
  print(x, ..., digits = digits, quote = quote, right = right, row.names = row.names, max = max)
  cat("\nGroups: ", paste(attr(x, "groups", exact = TRUE), collapse = ", "), "\n\n")
}
