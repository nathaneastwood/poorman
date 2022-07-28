### Taken from https://github.com/coolbutuseless/gluestick/blob/main/R/gluestick.R
### MIT License

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' String interpolation
#'
#' Expressions enclosed by specified delimiters will be evaluated as R code
#' within the context of the \code{src} data/environment.  The results will
#' then be inserted into the original string via \code{sprintf()}
#' i.e. string interpolation.
#'
#'
#' @param fmt single character string containing the format specification.
#' @param src data source. An \code{environment}, \code{list},
#'        \code{data.frame} or anything supported by \code{as.environment()}.
#'        Default: \code{parent.frame()} i.e. the calling environment
#' @param open,close the opening and closing character strings which delimit an expression.
#'        Default: \code{{}}.  Note: the delimiters can be more complex than
#'        just a single character
#' @param eval logical. Should the expressions be treated as R code to be
#'        evaluated? Default: TRUE means to treat the expressions as R code and
#'        evaluate.  If FALSE, then no code evaluation will ever be
#'        done and expressions will be treated as only variable
#'        names in the given \code{src} data.  This may be safer in some contexts
#'        e.g. for user supplied fmt strings.
#'
#' @examples
#' gluestick("Hello {name}", list(name = '#RStats'))
#' gluestick("Hello ~!name!~", list(name = '#RStats'), open = "~!", close = "!~")
#' name <- '#RStats'; gluestick("Hello {name}")
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

  nchar_open  <- nchar(open)
  nchar_close <- nchar(close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    is.character(fmt)
    length(fmt) == 1L
    is.character(open)
    length(open) == 1L
    nchar_open > 0L
    is.character(close)
    length(close) == 1
    nchar_close > 0
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Brute force the open/close characters into a regular expression for
  # extracting the expressions from the format string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  open  <- gsub("(.)", "\\\\\\1", open ) # Escape everything!!
  close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
  re    <- paste0(open, ".*?", close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the delimited expressions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  matches  <- gregexpr(re, fmt)
  exprs    <- regmatches(fmt, matches)[[1]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove the delimiters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create a valid sprintf fmt string.
  #  - replace all "{expr}" strings with "%s"
  #  - escape any '%' so sprintf() doesn't try and use them for formatting
  #    but only if the '%' is NOT followed by an 's'
  #
  # gluestick() doesn't deal with any pathological cases
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fmt_sprintf <- gsub(re      , "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl=TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (eval) {
    args <- lapply(exprs, function(expr) {eval(parse(text = expr), envir = src)})
  } else {
    args <- unname(mget(exprs, envir = as.environment(src)))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the string(s)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  do.call(sprintf, c(list(fmt_sprintf), args))
}
