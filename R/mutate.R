#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones; `transmute()` adds new variables and drops existing ones.
#' Both functions preserve the number of rows of the input. New variables overwrite existing variables of the same name.
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
#' @name mutate
#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#' @param .before,.after <[`poor-select`][select_helpers]> Optionally, control where new columns should appear (the
#' default is to add to the right hand side). See [relocate()] for more details.
#' @export
mutate.data.frame <- function(.data, ..., .before = NULL, .after = NULL) {
  conditions <- dotdotdot(..., .impute_names = TRUE)
  cond_nms <- names(dotdotdot(..., .impute_names = FALSE))
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

  context$.data
}

#' @export
mutate.grouped_data <- function(.data, ...) {
  rows <- rownames(.data)
  res <- apply_grouped_function("mutate", .data, drop = TRUE, ...)
  res[rows, , drop = FALSE]
}
