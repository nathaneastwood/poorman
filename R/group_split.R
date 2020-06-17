#' Split data.frame by groups
#'
#' @description
#' `group_split()` works like [base::split()] but
#'
#' * it uses the grouping structure from [group_by()] and is therefore subject to the data mask
#' * it does not name the elements of the list based on the grouping as this typically loses information and is
#'   confusing
#'
#' @details
#' **Grouped `data.frame`s:**
#'
#' The primary use case for `group_split()` is with already groups `data.frame`s, typically a result of [group_by()]. In
#' this case, `group_split()` only uses the first argument, the grouped `data.frame`, and warns when `...` is used.
#'
#' Because some of these groups may be empty, it is best paired with `group_keys()` which identifies the representatives
#' of each grouping variable for the group.
#'
#' **Ungrouped `data.frame`s:**
#'
#' When used on ungrouped `data.frame`s, `group_split()` forwards the `...` to `group_by()` before the split, therefore
#' the `...` are subject to the data mask.
#'
#' @param .data A `data.frame`.
#' @param ... Grouping specification, forwarded to [group_by()].
#' @param .keep `logical(1)`. Should the grouping columns be kept (default: TRUE)?
#'
#' @return
#' * `group_split()` returns a list of `data.frame`s. Each `data.frame` contains the rows of `.data` with the associated
#'   group and all the columns, including the grouping variables.
#' * `group_keys()` returns a `data.frame` with one row per group, and one column per grouping variable
#'
#' @seealso [group_by()]
#'
#' @examples
#' # Grouped data.frames:
#' mtcars %>% group_by(cyl, am) %>% group_split()
#' mtcars %>% group_by(cyl, am) %>% group_split(.keep = FALSE)
#' mtcars %>% group_by(cyl, am) %>% group_keys()
#'
#' # Ungrouped data.frames:
#' mtcars %>% group_split(am, cyl)
#'
#' @export
group_split <- function(.data, ..., .keep = TRUE) {
  dots_len <- ...length() > 0L
  if (has_groups(.data) && isTRUE(dots_len)) {
    warning("... is ignored in group_split(<grouped_df>), please use group_by(..., .add = TRUE) %>% group_split()")
  }
  if (!has_groups(.data) && isTRUE(dots_len)) {
    .data <- group_by(.data, ...)
  }
  if (!has_groups(.data) && isFALSE(dots_len)) {
    return(list(.data))
  }
  context$set_data(.data)
  on.exit(context$clean(), add = TRUE)
  groups <- get_groups(.data)
  attr(context$.data, "groups") <- NULL
  res <- split_into_groups(context$.data, groups)
  names(res) <- NULL
  if (isFALSE(.keep)) {
    res <- lapply(res, function(x) x[, !colnames(x) %in% groups])
  }
  any_empty <- unlist(lapply(res, function(x) !(nrow(x) == 0L)))
  res[any_empty]
}

#' @rdname group_split
#' @export
group_keys <- function(.data) {
  groups <- get_groups(.data)
  context$set_data(.data)
  res <- context$.data[, context$get_colnames() %in% groups, drop = FALSE]
  res <- res[!duplicated(res), , drop = FALSE]
  if (nrow(res) == 0L) return(res)
  class(res) <- "data.frame"
  res <- res[do.call(order, lapply(groups, function(x) res[, x])), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Split a `data.frame` into groups.
#'
#' @param .data A `data.frame`.
#' @param groups `character(n)`. A `vector` of grouping variables to split by.
#' @param drop `logical(1)`. Drop levels that do not occur?
#' @param ... Additional parameters to be passed to [split()].
#'
#' @return A `list` with a `data.frame` in each level.
#' @seealso [split()]
#' @noRd
split_into_groups <- function(.data, groups, drop = FALSE, ...) {
  class(.data) <- "data.frame"
  group_factors <- lapply(groups, function(x, .data) as.factor(.data[, x]), .data)
  split(x = .data, f = group_factors, drop = drop, ...)
}
