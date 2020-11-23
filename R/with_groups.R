#' Perform an operation with temporary groups
#'
#' This function allows you to modify the grouping variables for a single operation.
#'
#' @param .data A `data.frame`.
#' @param .groups <[`poor-select`][select_helpers]> One or more variables to group by. Unlike [group_by()], you can
#' only group by existing variables, and you can use `poor-select` syntax like `c(x, y, z)` to select multiple
#' variables.
#'
#' Use `NULL` to temporarily **un**group.
#' @param .f A `function` to apply to regrouped data. Supports lamba-style `~` syntax.
#' @param ... Additional arguments passed on to `.f`.
#'
#' @examples
#' df <- data.frame(g = c(1, 1, 2, 2, 3), x = runif(5))
#' df %>% with_groups(g, mutate, x_mean = mean(x))
#' df %>% with_groups(g, ~ mutate(.x, x_mean = mean(x)))
#'
#' df %>%
#'   group_by(g) %>%
#'   with_groups(NULL, mutate, x_mean = mean(x))
#'
#' # NB: grouping can't be restored if you remove the grouping variables
#' df %>%
#'   group_by(g) %>%
#'   with_groups(NULL, mutate, g = NULL)
#'
#' @export
with_groups <- function(.data, .groups, .f, ...) {
  cur_groups <- get_groups(.data)
  .groups <- eval_select_pos(.data = .data, .cols = substitute(.groups))
  val <- as_symbols(names(.data)[.groups])
  out <- do.call(group_by, c(list(.data = .data), val))
  .f <- as_function(.f)
  out <- .f(out, ...)
  out_groups <- cur_groups[cur_groups %in% colnames(out)]
  do.call(group_by, c(list(.data = out), as_symbols(out_groups)))
}
