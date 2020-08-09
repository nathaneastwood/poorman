#' Select Grouping Variables
#'
#' This selection helper matches grouping variables. It can be used within [select()] and [relocate()] selections.
#'
#' @seealso
#' [groups()] and [group_vars()] for retrieving the grouping variables outside selection contexts.
#'
#' @examples
#' mtcars %>% group_by(am, cyl) %>% select(group_cols())
#'
#' @export
group_cols <- function() {
  match(group_vars(select_env$.data), select_env$get_colnames())
}
