#' Group by one or more variables
#'
#' Determine the groups within a `data.frame` to perform operations on. [ungroup()] removes the grouping levels.
#'
#' @param .data `data.frame`. The data to group.
#' @param ... One or more unquoted column names to group/ungroup the data by.
#' @param .add `logical(1)`. When `FALSE` (the default) `group_by()` will override existing groups. To add to existing
#' groups, use `.add = TRUE`.
#' @param .drop `logical(1)`. Drop groups formed by factor levels that don't appear in the data? The default is `TRUE`
#' except when `.data` has been previously grouped with `.drop = FALSE`. See [group_by_drop_default()] for details.
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
group_by <- function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
  UseMethod("group_by")
}

#' @export
group_by.data.frame <- function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
  # The group columns are computed by `mutate()`, so pass on the frame of the user unless an outer verb has already
  if (is.null(context$group_env)) {
    context$group_env <- parent.frame()
    on.exit(context$group_env <- NULL, add = TRUE)
  }
  vars <- dotdotdot(..., .impute_names = TRUE, .env = context$group_env)
  if (all(vapply(vars, is.null, FALSE))) {
    res <- groups_set(.data, NULL)
    class(res) <- class(res)[!(class(res) %in% "grouped_df")]
    return(res)
  }
  new_cols <- add_group_columns(.data, vars)
  res <- new_cols$data
  groups <- new_cols$groups
  if (isTRUE(.add)) groups <- union(group_vars(.data), groups)
  unknown <- !(groups %in% colnames(res))
  if (any(unknown)) stop("Invalid groups: ", groups[unknown])
  if (length(groups) > 0L) {
    res <- groups_set(res, groups, .drop)
    class(res) <- union("grouped_df", class(res))
  }
  res
}

#' Default value for .drop argument of group_by
#'
#' @param .tbl A `data.frame`.
#'
#' @examples
#' group_by_drop_default(iris)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_by_drop_default()
#'
#' iris %>%
#'   group_by(Species, .drop = FALSE) %>%
#'   group_by_drop_default()
#'
#' @return `TRUE` unless `.tbl` is a grouped `data.frame` that was previously obtained by `group_by(.drop = FALSE)`
#'
#' @export
group_by_drop_default <- function(.tbl) {
  UseMethod("group_by_drop_default")
}

#' @export
group_by_drop_default.default <- function(.tbl) {
  TRUE
}

#' @export
group_by_drop_default.grouped_df <- function(.tbl) {
  tryCatch({
    !identical(attr(group_data(.tbl), ".drop"), FALSE)
  }, error = function(e) {
    TRUE
  })
}

# -- Helpers -------------------------------------------------------------------

#' Compute the columns to group by
#'
#' A group given as a bare column under its own name, e.g. `g`, is used as it is. Any other group, e.g. `h = x %/% 2`,
#' `tot = g` or `5`, is computed by `mutate()` on the ungrouped data.
#'
#' @param .data A `data.frame`.
#' @param vars A named `list` of unevaluated expressions, as returned by `dotdotdot(.impute_names = TRUE)`.
#' @return A `list` with `data`, `.data` with the computed columns, and `groups`, the names of the group columns.
#' @noRd
add_group_columns <- function(.data, vars) {
  vars <- vars[!vapply(vars, is.null, FALSE)]
  is_column <- vapply(seq_along(vars), function(i) identical(vars[[i]], as.name(names(vars)[i])), NA)
  if (!all(is_column)) {
    .data <- do.call(mutate, c(list(.data = ungroup(.data)), vars[!is_column]))
  }
  list(data = .data, groups = names(vars))
}
