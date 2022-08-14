### Taken from https://github.com/coolbutuseless/gluestick/blob/main/R/gluestick.R
### MIT License

#' String interpolation
#'
#' Expressions enclosed by specified delimiters will be evaluated as R code within the context of the `src`
#' data/environment. The results will then be inserted into the original string via `sprintf()` i.e. string
#' interpolation.
#'
#' @param fmt single character string containing the format specification.
#' @param src data source. An `environment`, `list`, `data.frame` or anything supported by [as.environment()]. Default:
#' [parent.frame()] i.e. the calling environment.
#' @param open,close the opening and closing character strings which delimit an expression. Default: `{}`. Note: the
#' delimiters can be more complex than just a single character.
#' @param eval `logical(1)`. Should the expressions be treated as R code to be evaluated? Default: `TRUE` means to treat
#' the expressions as R code and evaluate. If `FALSE`, then no code evaluation will ever be done and expressions will be
#' treated as only variable names in the given `src` data. This may be safer in some contexts e.g. for user supplied
#' `fmt` strings.
#'
#' @examples
#' gluestick("Hello {name}", list(name = '#RStats'))
#' gluestick("Hello ~!name!~", list(name = '#RStats'), open = "~!", close = "!~")
#' name <- '#RStats'; gluestick("Hello {name}")
#'
#' @noRd
gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

  nchar_open <- nchar(open)
  nchar_close <- nchar(close)

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

  open <- gsub("(.)", "\\\\\\1", open)
  close <- gsub("(.)", "\\\\\\1", close)
  re <- paste0(open, ".*?", close)

  matches <- gregexpr(re, fmt)
  exprs <- regmatches(fmt, matches)[[1]]

  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

  fmt_sprintf <- gsub(re, "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl = TRUE)

  args <- if (eval) {
    lapply(exprs, function(expr) eval(parse(text = expr), envir = src))
  } else {
    unname(mget(exprs, envir = as.environment(src)))
  }

  do.call(sprintf, c(list(fmt_sprintf), args))
}
