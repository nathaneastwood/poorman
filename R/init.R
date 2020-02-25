.onAttach <- function(libname, pkgname) {
  quote <- "\n  I'd seen my father. He was a poor man, and I watched him do astonishing things.\n    - Sidney Poitier"
  packageStartupMessage(quote)
}
