#' Subset columns using their names and types
#'
#' Select (and optionally rename) variables in a `data.frame`, using a concise mini-language that makes it easy to refer
#' to variables based on their name (e.g. `a:f` selects all columns from `a` on the left to `f` on the right). You can
#' also use predicate functions like [is.numeric()] to select variables based on their properties.
#'
#' ## Overview of selection features
#'
#' {poorman} selections implement a dialect of R where operators make it easy to select variables:
#'
#' - `:` for selecting a range of consecutive variables.
#' - `!` for taking the complement of a set of variables.
#' - `&` and `|` for selecting the intersection or the union of two sets of variables.
#' - `c()` for combining selections.
#'
#' In addition, you can use __selection helpers__. Some helpers select specific columns:
#' * [`everything()`]: Matches all variables.
#' * [`last_col()`]: Select last variable, possibly with an offset.
#'
#' These helpers select variables by matching patterns in their names:
#' * [`starts_with()`]: Starts with a prefix.
#' * [`ends_with()`]: Ends with a suffix.
#' * [`contains()`]: Contains a literal string.
#' * [`matches()`]: Matches a regular expression.
#' * [`num_range()`]: Matches a numerical range like `x01`, `x02`, `x03`.
#'
#' These helpers select variables from a character vector:
#' * [`all_of()`]: Matches variable names in a character vector. All names must be present, otherwise an out-of-bounds
#' error is thrown.
#' * [`any_of()`]: Same as `all_of()`, except that no error is thrown for names that don't exist.
#'
#' This helper selects variables with a function:
#' * [`where()`]: Applies a function to all variables and selects those for which the function returns `TRUE`.
#'
#' @param .data A `data.frame`.
#' @param ... <[`poor-select`][select_helpers]> One or more unquoted expressions separated by commas. Variable names can
#' be used as if they were positions in the data frame, so expressions like `x:y` can be used to select a range of
#' variables.
#'
#' @return
#' An object of the same type as `.data`. The output has the following properties:
#'
#' * Rows are not affected.
#' * Output columns are a subset of input columns, potentially with a different order. Columns will be renamed if
#' `new_name = old_name` form is used.
#' * Data frame attributes are preserved.
#' * Groups are maintained; you can't select off grouping variables.
#'
#' @examples
#' # Here we show the usage for the basic selection operators. See the
#' # specific help pages to learn about helpers like [starts_with()].
#'
#' # Select variables by name:
#' mtcars %>% select(mpg)
#'
#' # Select multiple variables by separating them with commas. Note
#' # how the order of columns is determined by the order of inputs:
#' mtcars %>% select(disp, gear, am)
#'
#' # Rename variables:
#' mtcars %>% select(MilesPerGallon = mpg, everything())
#'
#' # The `:` operator selects a range of consecutive variables:
#' select(mtcars, mpg:cyl)
#'
#' # The `!` operator negates a selection:
#' mtcars %>% select(!(mpg:qsec))
#' mtcars %>% select(!ends_with("p"))
#'
#' # `&` and `|` take the intersection or the union of two selections:
#' iris %>% select(starts_with("Petal") & ends_with("Width"))
#' iris %>% select(starts_with("Petal") | ends_with("Width"))
#'
#' # To take the difference between two selections, combine the `&` and
#' # `!` operators:
#' iris %>% select(starts_with("Petal") & !ends_with("Width"))
#'
#' @export
select <- function(.data, ...) {
  col_pos <- select_positions(.data, ..., .group_pos = TRUE)
  map_names <- names(col_pos)
  map_names_length <- nchar(map_names)
  if (any(map_names_length == 0L)) {
    no_new_names <- which(map_names_length == 0L)
    map_names[no_new_names] <- colnames(.data)[no_new_names]
  }
  res <- .data[, col_pos, drop = FALSE]
  if (!is.null(map_names) && all(col_pos > 0L)) colnames(res) <- map_names
  if (has_groups(.data)) res <- set_groups(res, get_groups(.data))
  res
}
