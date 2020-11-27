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
#' @param .data A `data.frame`.
#' @param ... Name-value pairs of expressions, each with length `1L`. The name of each argument will be the name of a
#' new column and the value will be its corresponding value. Use a `NULL` value in `mutate` to drop a variable. New
#' variables overwrite existing variables of the same name.
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
  conditions <- dotdotdot(..., .impute_names = TRUE)
  cond_nms <- names(dotdotdot(..., .impute_names = FALSE))
  used <- find_used(.data, conditions)
  if (length(conditions) == 0L) return(.data)
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  for (i in seq_along(conditions)) {
    not_named <- (is.null(cond_nms) || cond_nms[i] == "")
    res <- eval(conditions[[i]], envir = context$as_env())
    res_nms <- names(res)
    if (is.data.frame(res)) {
      if (not_named) {
        context$.data[, res_nms] <- res
      } else {
        context$.data[[cond_nms[i]]] <- res
      }
    } else if (is.atomic(res)) {
      context$.data[[names(conditions)[[i]]]] <- res
    } else {
      if (is.null(res_nms)) names(res) <- names(conditions)[[i]]
      context$.data[[names(res)]] <- res
    }
  }

  .before <- substitute(.before)
  .after <- substitute(.after)
  if (!is.null(.before) || !is.null(.after)) {
    new <- setdiff(cond_nms, names(.data))
    context$.data <- do.call(relocate, c(list(.data = context$.data), new, .before = .before, .after = .after))
  }

  if (keep == "all") {
    context$.data
  } else if (keep == "unused") {
    unused <- setdiff(colnames(.data), used)
    keep <- intersect(context$get_colnames(), c(group_vars(.data), unused, cond_nms))
    do.call(select, c(list(.data = context$.data), keep))
  } else if (keep == "used") {
    keep <- intersect(context$get_colnames(), c(group_vars(.data), used, cond_nms))
    do.call(select, c(list(.data = context$.data), keep))
  } else if (keep == "none") {
    keep <- c(setdiff(group_vars(.data), cond_nms), intersect(cond_nms, context$get_colnames()))
    do.call(select, c(list(.data = context$.data), keep))
  }
}

#' @export
mutate.grouped_data <- function(.data, ...) {
  rows <- rownames(.data)
  res <- apply_grouped_function("mutate", .data, drop = TRUE, ...)
  res[rows, , drop = FALSE]
}

## -- Helpers ----------------------------------------------------------------------------------------------------------

find_used <- function(.data, conditions) {
  inputs <- do.call(c, lapply(conditions, as.character))
  unique(inputs[inputs %in% colnames(.data)])
}
