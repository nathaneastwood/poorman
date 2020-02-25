extract <- `[`
extract2 <- `[[`
inset <- `[<-`
set_colnames <- `colnames<-`

deparse_dots <- function(...) {
  vapply(substitute(...()), deparse, NA_character_)
}
