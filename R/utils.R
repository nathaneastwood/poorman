extract <- `[`
extract2 <- `[[`
inset <- `[<-`

`[.grouped_df` <- function(x, i, j, drop = if (missing(i)) TRUE else length(cols) == 1) {
  groups <- attr(x, "groups", exact = TRUE)
  class(x) <- "data.frame"
  res <- extract(x, i, j, drop)
  class(res) <- c("grouped_df", class(x))
  attr(res, "groups") <- groups
  res
}

deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}
