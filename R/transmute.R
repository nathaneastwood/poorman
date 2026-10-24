#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  env <- parent.frame()
  mutate_cols(.data, dotdotdot(..., .env = env), env, keep = "none")
}
