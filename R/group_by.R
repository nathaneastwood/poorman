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
#' @section Extension classes:
#' On grouped data, `mutate()`, `transmute()`, `filter()`, `arrange()`, `slice()`, `select()`, `rename()`, `distinct()`
#' and the other verbs which modify their input keep its class, including a subclass of a grouped `data.frame` added by
#' another package. This differs from `dplyr` 1.0.10, which drops such a subclass. A method for an extension class
#' which calls `NextMethod()`, e.g. `mutate.myclass <- function(.data, ...) NextMethod()`, gets back its own class and
#' should not add it again. `summarise()` and `count()` build a new `data.frame`, so their result does not keep the
#' subclass.
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
  env <- parent.frame()
  vars <- dotdotdot(..., .impute_names = TRUE, .env = env)
  if (all(vapply(vars, is.null, FALSE))) {
    res <- groups_set(.data, NULL)
    class(res) <- class(res)[!(class(res) %in% "grouped_df")]
    return(res)
  }
  new_cols <- add_group_columns(.data, vars, env)
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
#' `tot = g` or `5`, is computed as by `mutate()` on the ungrouped data.
#'
#' @param .data A `data.frame`.
#' @param vars A named `list` of unevaluated expressions, as returned by `dotdotdot(.impute_names = TRUE)`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with `data`, `.data` with the computed columns, and `groups`, the names of the group columns.
#' @noRd
add_group_columns <- function(.data, vars, env) {
  vars <- vars[!vapply(vars, is.null, FALSE)]
  is_column <- vapply(seq_along(vars), function(i) identical(vars[[i]], as.name(names(vars)[i])), NA)
  if (!all(is_column)) {
    .data <- mutate_df(ungroup(.data), vars[!is_column], env)$data
  }
  list(data = .data, groups = names(vars))
}

#' Group by expressions given to another verb
#'
#' `group_by()` looks up variables in the frame which calls it. A verb which groups its own `...`, e.g. `count()`,
#' calls this rather than `group_by()`, so that the variables are looked up in the frame of the user, which calls the
#' verb. `group_by()` is still called through its generic, so that a method for another class is used.
#'
#' The verb passes its own `parent.frame()` as `.env`, rather than this function finding the frame by counting frames
#' up the stack, so that a wrapper added between the verb and this function cannot change where variables are found.
#' The argument is named with a dot so that it cannot take a group of the user named `env`.
#'
#' @param .data A `data.frame`.
#' @param ... The `...` of the verb.
#' @param .add As `group_by()`.
#' @param .env The `environment` of the caller of the verb, in which `group_by()` is called.
#' @return The result of `group_by()`.
#' @noRd
group_by_caller <- function(.data, ..., .add = FALSE, .env) {
  do.call(group_by, c(list(.data), eval(substitute(alist(...))), .add = .add), envir = .env)
}
