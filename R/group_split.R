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
#' The primary use case for `group_split()` is with already grouped `data.frame`s, typically a result of [group_by()].
#' In this case, `group_split()` only uses the first argument, the grouped `data.frame`, and warns when `...` is used.
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
  dots_len <- length(dotdotdot(...)) > 0L
  if (has_groups(.data) && isTRUE(dots_len)) {
    warning("... is ignored in group_split(<grouped_df>), please use group_by(..., .add = TRUE) %>% group_split()")
  }
  if (!has_groups(.data) && isTRUE(dots_len)) {
    .data <- group_by_caller(.data, ..., .env = parent.frame())
  }
  if (!has_groups(.data) && !isTRUE(dots_len)) {
    return(list(.data))
  }
  groups <- group_vars(.data)
  rows <- get_groups(.data, groups)[[".rows"]]
  .data <- ungroup(.data)
  if (!isTRUE(.keep)) .data <- .data[, !colnames(.data) %in% groups, drop = FALSE]
  lapply(rows, function(i) subset_rows(.data, i))
}

#' @rdname group_split
#' @export
group_keys <- function(.data) {
  groups <- group_vars(.data)
  if (length(groups) == 0L) {
    return(new_data_frame(list(), 1L))
  }
  res <- get_groups(.data, groups)
  res[[".rows"]] <- NULL
  attr(res, ".drop") <- NULL
  res
}
