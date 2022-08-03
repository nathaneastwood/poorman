#' Separate a character column into multiple columns with a regular expression
#' or numeric locations
#'
#' Given either a regular expression or a vector of character positions,
#' `separate()` turns a single character column into multiple columns.
#'
#' @param data A `data.frame`.
#' @param col Column name or position.
#' @param into Names of new variables to create as character vector. Use `NA` to
#' omit the variable in the output.
#' @param sep Separator between columns.
#'
#' If character, `sep` is interpreted as a regular expression. The default value
#' is a regular expression that matches any sequence of non-alphanumeric values.
#'
#' If numeric, `sep` is interpreted as character positions to split at. Positive
#' values start at 1 at the far-left of the string; negative value start at -1
#' at the far-right of the string. The length of `sep` should be one less than
#' `into`.
#' @param remove If `TRUE`, remove input column from output data frame.
#'
#' @param convert If `TRUE`, will run `type.convert()` with `as.is = TRUE` on new
#' columns. This is useful if the component columns are integer, numeric or logical.
#'
#' NB: this will cause string "`NA`"s to be converted to `NA`s.
#'
#' @param extra If `sep` is a character vector, this controls what happens when
#' there are too many pieces. There are three valid options:
#' * "warn" (the default): emit a warning and drop extra values.
#' * "drop": drop any extra values without a warning.
#' * "merge": only splits at most `length(into`) times
#'
#' @param fill If `sep` is a character vector, this controls what happens when
#' there are not enough pieces. There are three valid options:
#' * "warn" (the default): emit a warning and fill from the right
#' * "right": fill with missing values on the right
#' * "left": fill with missing values on the left
#'
#' @param ... Additional arguments passed on to methods.
#'
#' @return A `data.frame`.
#'
#' @examples
#' # If you want to split by any non-alphanumeric value (the default):
#' df <- data.frame(x = c(NA, "x.y", "x.z", "y.z"))
#' df %>% separate(x, c("A", "B"))
#'
#' # If you just want the second variable:
#' df %>% separate(x, c(NA, "B"))
#'
#' # If every row doesn't split into the same number of pieces, use
#' # the extra and fill arguments to control what happens:
#' df <- data.frame(x = c("x", "x y", "x y z", NA))
#' df %>% separate(x, c("a", "b"))
#' # The same behaviour as previous, but drops the c without warnings:
#' df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
#' # Opposite of previous, keeping the c and filling left:
#' df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")
#' # Or you can keep all three:
#' df %>% separate(x, c("a", "b", "c"))
#'
#' # To only split a specified number of times use extra = "merge":
#' df <- data.frame(x = c("x: 123", "y: error: 7"))
#' df %>% separate(x, c("key", "value"), ": ", extra = "merge")
#'
#' # Use regular expressions to separate on multiple characters:
#' df <- data.frame(x = c(NA, "a1b", "c4d", "e9g"))
#' df %>% separate(x, c("A","B"), sep = "[0-9]")
#'
#' # convert = TRUE detects column classes:
#' df <- data.frame(x = c("x:1", "x:2", "y:4", "z", NA))
#' df %>% separate(x, c("key","value"), ":") %>% str
#' df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% str
#'
#' @export

separate <- function(
    data,
    col,
    into,
    sep = "[^[:alnum:]]+",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn",
    ...
) {

  col <- deparse_var(col)

  stopifnot(length(col) == 1)
  stopifnot(is.character(into))

  # split each value of "col" with the pattern
  if (is.character(sep)) {
    splitted <- strsplit(data[[col]], sep)
  } else if (is.numeric(sep)) {
    splitted <- lapply(df[[col]], function(x) {
      substring(x, c(1, sep + 1), c(sep, nchar(x)))
    })
  }

  # are there too many pieces compared to the size of "into"?
  too_long <- unlist(lapply(1:length(splitted), function(x) {
    if (length(splitted[[x]]) > length(into) || length(sep) >= length(into)) return(x)
  }))

  if (length(too_long) > 0) {
    if (extra == "warn") {
      warning(
        paste0(
          "Expected ", length(into), " pieces. Additional pieces discarded in ",
          length(too_long), " rows [", paste(too_long, collapse = ", "), "]."
        ),
        call. = FALSE
      )
    } else if (extra == "merge") {
      for (i in too_long) {
        # need to find exactly what was removed by strsplit() so that we can
        # plug it back
        ### ???

        # merge last valid elements with all elements that come after
        tmp <- splitted[[i]][1:length(into)]
        tmp[length(into)] <- paste0(
          splitted[[i]][length(into):length(splitted[[i]])],
          collapse = ""
        )
        splitted[[i]] <- tmp
      }
    }
  }


  # create the new cols
  for (i in 1:length(into)) {

    # if NA in "into", don't create the column
    if (is.na(into[[i]])) next

    data[[into[i]]] <- unlist(lapply(splitted, function(x) {
      if (all(is.na(x))) return(NA)
      return(x[i])
    }))

    if (isTRUE(convert)) {
      data[[into[i]]] <- type.convert(data[[into[i]]], as.is = TRUE)
    }

  }

  if (isTRUE(remove)) data[[col]] <- NULL

  data

}
