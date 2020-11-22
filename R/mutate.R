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
#' @name mutate
#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @export
mutate.data.frame <- function(.data, ...) {
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
  context$.data
}

#' @export
mutate.grouped_data <- function(.data, ...) {
  rows <- rownames(.data)
  res <- apply_grouped_function("mutate", .data, drop = TRUE, ...)
  res[rows, , drop = FALSE]
}
