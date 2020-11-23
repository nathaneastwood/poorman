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
#' # You can group by expressions: this is just short-hand for
#' # a mutate() followed by a group_by()
#' mtcars %>% group_by(vsam = vs + am)
#'
#' @return
#' When using [group_by()], a `data.frame`, grouped by the grouping variables.
#'
#' @name group_by
#' @export
group_by <- function(.data, ..., .add = FALSE) {
  UseMethod("group_by")
}

#' @export
group_by.data.frame <- function(.data, ..., .add = FALSE) {
  vars <- dotdotdot(..., .impute_names = TRUE)
  if (all(vapply(vars, is.null, FALSE))) {
    res <- set_groups(.data, NULL)
    class(res) <- class(res)[!(class(res) %in% "grouped_data")]
    return(res)
  }
  new_cols <- add_group_columns(.data, vars)
  res <- new_cols$data
  groups <- new_cols$groups
  if (isTRUE(.add)) groups <- union(group_vars(.data), groups)
  unknown <- !(groups %in% colnames(res))
  if (any(unknown)) stop("Invalid groups: ", groups[unknown])
  if (length(groups) > 0L) {
    res <- set_groups(res, groups)
    class(res) <- union("grouped_data", class(res))
    res
  } else {
    res
  }
}

add_group_columns <- function(.data, vars) {
  vars <- vars[!vapply(vars, is.null, FALSE)]
  types <- do.call(c, lapply(vars, typeof))
  test <- any(types == "language")
  needs_mutate <- if (test) unname(which(types == "language")) else NULL
  if (!is.null(needs_mutate)) {
    .data <- do.call(mutate, c(list(.data = ungroup(.data)), vars[needs_mutate]))
  }
  list(data = .data, groups = names(vars))
}

#' @param x A `data.frame`.
#' @return
#' When using [ungroup()], a `data.frame`.
#' @rdname group_by
#' @export
ungroup <- function(x, ...) {
  UseMethod("ungroup")
}

#' @export
ungroup.data.frame <- function(x, ...) {
  rm_groups <- deparse_dots(...)
  groups <- get_groups(x)
  if (length(rm_groups) == 0L) rm_groups <- groups
  x <- set_groups(x, groups[!(groups %in% rm_groups)])
  if (length(attr(x, "groups")) == 0L) {
    attr(x, "groups") <- NULL
    class(x) <- class(x)[!(class(x) %in% "grouped_data")]
  }
  x
}

#' @export
ungroup.grouped_data <- function(x, ...) {
  NextMethod("ungroup")
}

#' Determine the grouping structure of the data
#'
#' @noRd
set_groups <- function(x, groups) {
  attr(x, "groups") <- if (is.null(groups) || length(groups) == 0L) {
    NULL
  } else {
    group_data_worker(x, groups)
  }
  x
}

get_groups <- function(x) {
  groups <- attr(x, "groups", exact = TRUE)
  if (is.null(groups)) character(0) else colnames(groups)[!colnames(groups) %in% c(".group_id", ".rows")]
}

get_group_details <- function(x) {
  groups <- attr(x, "groups", exact = TRUE)
  if (is.null(groups)) character(0) else groups
}

has_groups <- function(x) {
  groups <- get_groups(x)
  if (length(groups) == 0L) FALSE else TRUE
}

#' @param fn `character(1)`. The function to apply to each group.
#' @param .data A `data.frame`.
#' @param ... Arguments to be passed to `fn`.
#' @noRd
apply_grouped_function <- function(fn, .data, drop = FALSE, ...) {
  groups <- get_groups(.data)
  grouped <- split_into_groups(.data, groups, drop)
  res <- do.call(rbind, unname(lapply(grouped, fn, ...)))
  if (any(groups %in% colnames(res))) {
    class(res) <- c("grouped_data", class(res))
    res <- set_groups(res, groups[groups %in% colnames(res)])
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
#' @noRd
print.grouped_data <- function(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {
  class(x) <- "data.frame"
  print(x, ..., digits = digits, quote = quote, right = right, row.names = row.names, max = max)
  cat("\nGroups: ", paste(get_groups(x), collapse = ", "), "\n\n")
}
