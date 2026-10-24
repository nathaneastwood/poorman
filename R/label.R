#' Name an unnamed expression
#'
#' The automatic name of an unnamed expression, as used by `summarise()` and for the default `name` of `nest_join()`.
#' Short expressions are named by their one-line deparse. Long ones are abbreviated with `...` so that the name stays
#' readable:
#'
#' * A symbol is named after itself, and `.data$x` and `.data[["x"]]` after the column `x`.
#' * A binary operator call longer than 60 characters keeps its operator and as many of its operands as fit, the left
#'   one first, e.g. `... %>% select(a, b)`. An operand which R deparses over several lines, such as a `{` block, is
#'   always replaced by `...`.
#' * Any other call is abbreviated when R itself deparses it over several lines. It keeps the head of the call, e.g.
#'   `mean(...)`, `{ ... }` or `function(x) ...`.
#'
#' When `env` is given, the subscript of each `.data[[...]]` is first replaced by its value in `env`, so that with
#' `var <- "x"`, `.data[[var]]` is named `x` and `mean(.data[[var]])` is named `mean(.data[["x"]])`.
#'
#' The names match those of `dplyr`, except that the limit of 60 characters is fixed and counts characters rather than
#' bytes, and that `?` and `:=` are treated as prefix calls, as R prints them.
#'
#' @param expr An unevaluated expression.
#' @param env An `environment` in which to evaluate the subscripts of `.data[[...]]`, or `NULL` to leave them as they
#' are.
#' @return `character(1)`. The name.
#' @noRd
auto_label <- function(expr, env = NULL) {
  if (!is.null(env)) expr <- inline_data_subscripts(expr, env)
  if (is.symbol(expr)) return(as.character(expr))
  if (!is.call(expr)) return(one_line(expr))
  col <- data_pronoun_name(expr)
  if (!is.null(col)) return(col)

  line <- one_line(expr)
  # Operators are found by asking R's parser rather than by listing them, so that anything R prints in prefix form
  # (`?`, `:=`, subsetting, unary operators) is not mistaken for a binary operator
  parts <- split_binary_call(line)
  if (!is.null(parts)) {
    # An operand that R wraps (a `{` block, say) is never shown, however short its one-line text is
    wraps <- vapply(as.list(expr)[2:3], function(x) length(deparse(x, width.cutoff = 60L)) > 1L, NA)
    return(if (nchar(line) > 60L || any(wraps)) abbreviate_binary(parts, wraps) else line)
  }
  # Any other call is abbreviated exactly when R itself would wrap it over several lines
  if (length(deparse(expr, width.cutoff = 60L)) > 1L) abbreviate_call(expr) else line
}

#' Deparse an expression to a single line
#'
#' @param expr An R object.
#' @return `character(1)`. The deparsed expression, with any lines R still breaks joined by a space.
#' @noRd
one_line <- function(expr) paste(deparse(expr, width.cutoff = 500L), collapse = " ")

#' Name a column taken from the `.data` pronoun
#'
#' `.data$x` and `.data[["x"]]` are named after the column, but only when they are the whole expression.
#'
#' @param expr A call.
#' @return `character(1)`. The name of the column, or `NULL` when `expr` does not take a column from `.data`.
#' @noRd
data_pronoun_name <- function(expr) {
  if (length(expr) != 3L || !identical(expr[[2L]], as.name(".data"))) return(NULL)
  col <- expr[[3L]]
  if (identical(expr[[1L]], as.name("$")) && is.symbol(col)) return(as.character(col))
  if (identical(expr[[1L]], as.name("[[")) && is.character(col) && length(col) == 1L) col else NULL
}

#' Replace the subscripts of `.data[[...]]` by their values
#'
#' A subscript which is not a literal, e.g. `var` in `.data[[var]]`, is evaluated in `env`. It is replaced when its
#' value is a single atomic value, and left as it is otherwise, including when it can't be evaluated.
#'
#' @param expr An unevaluated expression.
#' @param env An `environment` in which to evaluate the subscripts.
#' @return `expr`, with the subscripts replaced.
#' @noRd
inline_data_subscripts <- function(expr, env) {
  if (!is.call(expr)) return(expr)
  if (is_computed_data_subscript(expr)) {
    value <- tryCatch(eval(expr[[3L]], env), error = function(e) NULL)
    if (is.atomic(value) && length(value) == 1L) expr[[3L]] <- value
    return(expr)
  }
  for (i in seq_along(expr)[-1L]) {
    if (is.call(expr[[i]])) expr[[i]] <- inline_data_subscripts(expr[[i]], env)
  }
  expr
}

#' Test whether a call takes a column from `.data` by a computed subscript
#'
#' @param expr A call.
#' @return `logical(1)`. `TRUE` for `.data[[i]]` where `i` is a symbol or a call rather than a literal.
#' @noRd
is_computed_data_subscript <- function(expr) {
  length(expr) == 3L && identical(expr[[1L]], as.name("[[")) && identical(expr[[2L]], as.name(".data")) &&
    (is.call(expr[[3L]]) || is.symbol(expr[[3L]]))
}

#' Split a binary operator call into its parts
#'
#' The one-line deparse of a call is parsed again to find out whether R reads it as a binary operator call. Operand
#' text is cut from the line so that it keeps any brackets R put in. Only the columns of terminal tokens are used,
#' because the columns of expression nodes can count bytes rather than characters.
#'
#' @param line `character(1)`. The one-line deparse of a call, as given by `one_line()`.
#' @return A `list` with the text of the left operand `lhs`, the operator `op` and the right operand `rhs`, or `NULL`
#' when `line` is not a binary operator call.
#' @noRd
split_binary_call <- function(line) {
  # The parser counts bytes rather than characters in text it does not know to be UTF-8
  if (l10n_info()[["UTF-8"]]) Encoding(line) <- "UTF-8"
  data <- tryCatch(utils::getParseData(parse(text = line, keep.source = TRUE)), error = function(e) NULL)
  if (is.null(data)) return(NULL)
  top <- data$id[data$parent == 0L]
  if (length(top) != 1L) return(NULL)
  kids <- data[data$parent == top, ]
  kids <- kids[order(kids$col1), ]
  if (!has_binary_shape(kids$token)) return(NULL)
  terminals <- data[data$terminal, ]
  op_start <- kids$col1[2L]
  op_end <- kids$col2[2L]
  list(
    lhs = operand_text(line, terminals, 1L, op_start - 1L),
    op = kids$text[2L],
    rhs = operand_text(line, terminals, op_end + 1L, nchar(line))
  )
}

#' Test whether parse data has the shape of a binary operator call
#'
#' A binary operator call has an operand on each side of an operator. `$` and `@` have a name rather than an
#' expression on the right.
#'
#' @param tokens `character(n)`. The tokens of the children of the top-level node, in the order of the line.
#' @return `logical(1)`.
#' @noRd
has_binary_shape <- function(tokens) {
  length(tokens) == 3L && tokens[1L] == "expr" && tokens[2L] != "expr" &&
    tokens[3L] %in% c("expr", "SYMBOL", "SLOT", "STR_CONST")
}

#' Cut the text of an operand from a line
#'
#' R prints `/`, `%%` and `%/%` without spaces, but an abbreviated label shows them with spaces, so they are spaced
#' wherever they occur in the operand.
#'
#' @param line `character(1)`. The one-line deparse of the whole call.
#' @param terminals A `data.frame`. The terminal tokens of the parse data of `line`.
#' @param from,to `integer(1)`. The first and last characters of the operand in `line`.
#' @return `character(1)`. The text of the operand, without surrounding spaces.
#' @noRd
operand_text <- function(line, terminals, from, to) {
  text <- substr(line, from, to)
  ops <- terminals[terminals$col1 >= from & terminals$col2 <= to & terminals$text %in% c("/", "%%", "%/%"), ]
  for (i in rev(order(ops$col1))) {
    start <- ops$col1[i] - from + 1L
    end <- ops$col2[i] - from + 1L
    text <- paste0(substr(text, 1L, start - 1L), " ", ops$text[i], " ", substr(text, end + 1L, nchar(text)))
  }
  gsub("^ +| +$", "", text)
}

#' Abbreviate a binary operator call
#'
#' The left operand has priority: the label is the first of `lhs op ...`, `... op rhs` and `... op ...` which fits in
#' 60 characters. Each `...` costs three characters.
#'
#' @param parts A `list` as returned by `split_binary_call()`.
#' @param wraps `logical(2)`. Whether R deparses each operand over several lines, in which case it is never shown.
#' @return `character(1)`. The label.
#' @noRd
abbreviate_binary <- function(parts, wraps) {
  op <- if (parts$op %in% c("^", ":", "$", "@")) parts$op else paste0(" ", parts$op, " ")
  if (!wraps[1L] && nchar(parts$lhs) + nchar(op) + 3L <= 60L) return(paste0(parts$lhs, op, "..."))
  if (!wraps[2L] && 3L + nchar(op) + nchar(parts$rhs) <= 60L) return(paste0("...", op, parts$rhs))
  paste0("...", op, "...")
}

#' Abbreviate a call other than a binary operator call
#'
#' The head of the call is kept and the rest is replaced by `...`.
#'
#' @param expr A call which R deparses over several lines.
#' @return `character(1)`. The label.
#' @noRd
abbreviate_call <- function(expr) {
  head <- expr[[1L]]
  name <- if (is.symbol(head)) as.character(head) else ""
  if (length(expr) == 2L && name %in% c("-", "+", "!", "~")) return(paste0(name, "..."))
  if (!nzchar(name)) return(paste0(one_line(head), "(...)"))
  switch(
    name,
    "(" = "(...)",
    "[" = "...[]",
    "[[" = "...[[]]",
    "{" = "{ ... }",
    "if" = "if (...) NULL",
    "for" = "for (... in NULL) NULL",
    "while" = "while (...) NULL",
    "repeat" = "repeat ...",
    "function" = abbreviate_function(expr),
    paste0(paste(deparse(head, backtick = TRUE), collapse = " "), "(...)")
  )
}

#' Abbreviate a function definition
#'
#' The header is deparsed with an empty body, which gives the same wrapping as the whole function. When the formals
#' wrap, only the first line is kept.
#'
#' @param expr A call to `function`.
#' @return `character(1)`. The label, e.g. `function(x) ...`.
#' @noRd
abbreviate_function <- function(expr) {
  header <- deparse(as.call(list(as.name("function"), expr[[2L]], NULL, NULL)), width.cutoff = 60L)
  if (length(header) == 1L) sub("NULL$", "...", header) else paste0(header[1L], "...")
}
