#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are `TRUE`.
#'
#' @param .data A `data.frame`.
#' @param ... Logical predicated defined in terms of the variables in `.data`. Multiple conditions are combined with
#' `&`. Arguments within `...` are automatically quoted and evaluated within the context of the `data.frame`.
#'
#' @section Useful filter functions:
#'
#' * `==`, `>`, `>=`, etc.
#' * `&`, `|`, `!`, `xor()`
#' * `is.na()`
#'
#' @examples
#' filter(mtcars, am == 1)
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#'
#' @return
#' A `data.frame`.
#'
#' @export
filter <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("filter")
}

#' @export
filter.default <- function(.data, ...) {
  conditions <- eval(substitute(alist(...)))
  cond_class <- vapply(conditions, typeof, NA_character_)
  if (any(cond_class != "language")) stop("Conditions must be logical vectors")
  context$.data <- .data
  on.exit(rm(.data, envir = context), add = TRUE)
  eval_env$env <- parent.frame()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  rows <- lapply(
    conditions,
    function(cond, frame) eval(cond, context$.data, frame),
    frame = eval_env$env
  )
  rows <- Reduce("&", rows)
  .data[rows & !is.na(rows), ]
}

#' @export
filter.grouped_data <- function(.data, ...) {
  rows <- rownames(.data)
  res <- apply_grouped_function("filter", .data, ...)
  res[rows[rows %in% rownames(res)], ]
}
