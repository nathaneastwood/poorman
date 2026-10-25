#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones; `transmute()` adds new variables and drops existing ones.
#' Both functions preserve the number of rows of the input. New variables overwrite existing variables of the same name.
#' Variables can be removed by setting their value to `NULL`.
#'
#' @section Useful mutate functions:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()], [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummin()], [cummax()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @section Variables made inside expressions:
#' A variable assigned inside an expression, e.g. `{ tmp <- x * 2; tmp + 1 }`, can be used by the later expressions of
#' the same call. On grouped data, each group has its own variables, so a variable assigned while evaluating one group
#' is never seen by the next group. This differs from `dplyr` 1.0.10, where such a variable can leak into the groups
#' evaluated after it.
#'
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of expressions, each with length `1L`. The name of each argument will be the name of a
#' new column and the value will be its corresponding value. Use a `NULL` value in `mutate` to drop a variable. New
#' variables overwrite existing variables of the same name.
#'
#' A name can also be given with `:=`, e.g. `total := sum(x)`. A string on the left of `:=` is a `gluestick` template
#' which is filled in from the calling environment, so that a name can be built from a variable, e.g.
#' `"{var}_mean" := mean(x)`.
#'
#' @examples
#' mutate(mtcars, mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2)
#' mtcars %>% mutate(mpg2 = mpg * 2, cyl2 = cyl * 2)
#'
#' # Newly created variables are available immediately
#' mtcars %>% mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2)
#'
#' # You can also use mutate() to remove variables and modify existing variables
#' mtcars %>% mutate(
#'   mpg = NULL,
#'   disp = disp * 0.0163871 # convert to litres
#' )
#'
#' # By default, new columns are placed on the far right.
#' # You can override this with `.before` or `.after`.
#' df <- data.frame(x = 1, y = 2)
#' df %>% mutate(z = x + y)
#' df %>% mutate(z = x + y, .before = 1)
#' df %>% mutate(z = x + y, .after = x)
#'
#' # By default, mutate() keeps all columns from the input data.
#' # You can override with `.keep`
#' df <- data.frame(
#'   x = 1, y = 2, a = "a", b = "b",
#'   stringsAsFactors = FALSE
#' )
#' df %>% mutate(z = x + y, .keep = "all") # the default
#' df %>% mutate(z = x + y, .keep = "used")
#' df %>% mutate(z = x + y, .keep = "unused")
#' df %>% mutate(z = x + y, .keep = "none") # same as transmute()
#'
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
#' @name mutate
#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#' @param .keep  This argument allows you to control which columns from `.data` are retained in the output:
#'
#' * `"all"`, the default, retains all variables.
#' * `"used"` keeps any variables used to make new variables; it's useful for checking your work as it displays inputs
#'   and outputs side-by-side.
#' * `"unused"` keeps only existing variables **not** used to make new variables.
#' * `"none"`, only keeps grouping keys (like [transmute()]).
#'
#' Grouping variables are always kept, unconditional to `.keep`.
#' @param .before,.after <[`poor-select`][select_helpers]> Optionally, control where new columns should appear (the
#' default is to add to the right hand side). See [relocate()] for more details.
#' @export
mutate.data.frame <- function(
  .data,
  ...,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
) {
  keep <- match.arg(arg = .keep, choices = c("all", "used", "unused", "none"), several.ok = FALSE)
  env <- parent.frame()
  mutate_cols(
    .data = .data,
    dots = dotdotdot(..., .env = env),
    env = env,
    keep = keep,
    before = substitute(.before),
    after = substitute(.after)
  )
}

## -- Helpers ----------------------------------------------------------------------------------------------------------

#' Add new columns and keep the requested columns
#'
#' The body of `mutate()`, which takes its expressions and environment explicitly so that verbs built on it, e.g.
#' `transmute()`, can evaluate in the frame of their own caller.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @param keep `character(1)`. One of `"all"`, `"used"`, `"unused"` or `"none"`, as `mutate(.keep)`.
#' @param before,after Unevaluated `.before` and `.after` arguments of `mutate()`.
#' @return A `data.frame`.
#' @noRd
mutate_cols <- function(.data, dots, env, keep = "all", before = NULL, after = NULL) {
  res <- mutate_df(.data = .data, dots = dots, env = env)
  data <- res$data
  new_cols <- res$new_cols

  if (!is.null(before) || !is.null(after)) {
    new <- setdiff(new_cols, names(.data))
    data <- do.call(relocate, c(list(.data = data), new, .before = before, .after = after))
  }

  if (keep == "all") {
    data
  } else if (keep == "unused") {
    unused <- setdiff(colnames(.data), res$used_cols)
    keep <- intersect(colnames(data), c(group_vars(.data), unused, new_cols))
    select(.data = data, keep)
  } else if (keep == "used") {
    keep <- intersect(colnames(data), c(group_vars(.data), res$used_cols, new_cols))
    select(.data = data, keep)
  } else if (keep == "none") {
    keep <- c(setdiff(group_vars(.data), new_cols), intersect(new_cols, colnames(data)))
    select(.data = data, keep)
  }
}

#' Compute new columns
#'
#' The expressions are evaluated for each group by `eval_groups()`. The results of each are combined across the groups
#' by `combine_groups()` and put back into the order of the rows.
#'
#' @param .data A `data.frame`.
#' @param dots A `list` of unevaluated expressions, as returned by `dotdotdot()`.
#' @param env An `environment` in which to look up variables that are not columns.
#' @return A `list` with `data`, `.data` with the new columns, `used_cols`, the columns of `.data` used by the
#' expressions, and `new_cols`, the names of the new columns.
#' @noRd
mutate_df <- function(.data, dots, env) {
  if (length(dots) == 0L) return(list(data = .data, used_cols = NULL, new_cols = NULL))
  used <- unname(do.call(c, lapply(dots, find_used)))
  used <- used[used %in% colnames(.data)]
  res <- eval_groups(.data, dots, env, group_size_rule, drop_null = TRUE)
  order <- ungroup_order(res$rows, .row_names_info(.data, 2L))
  new_cols <- character()
  for (i in seq_along(dots)) {
    col <- combine_groups(res$values[[i]], res$sizes, res$labels[[i]])
    if (!is.null(order)) col <- vec_slice(col, order)
    if (res$unnamed[[i]] && is.data.frame(col)) {
      for (j in names(col)) .data[[j]] <- .subset2(col, j)
      new_cols <- c(new_cols, names(col))
    } else {
      .data[[res$labels[[i]]]] <- col
      new_cols <- c(new_cols, res$labels[[i]])
    }
  }
  groups <- group_vars(.data)
  if (any(groups %in% new_cols)) .data <- groups_set(.data, groups)
  list(data = .data, used_cols = used, new_cols = new_cols)
}

#' Recursively find used variables in an expression
#'
#' @param expr An expression.
#'
#' @return `character(n)`.
#'
#' @examples
#' \dontrun{
#' expr <- quote(x * var + ifelse(x < y, 1, 0) + n())
#' find_used(expr)
#' }
#'
#' @author Mark T Fairbanks, \email{mark.t.fairbanks@@gmail.com}
#'
#' @noRd
find_used <- function(expr) {
  if (is.symbol(expr)) {
    as.character(expr)
  } else {
    unique(unlist(lapply(expr[-1], find_used)))
  }
}
