#' Fill in missing values with previous or next value
#'
#' Fills missing values in selected columns using the next or previous entry. This is useful in the common output format
#' where values are not repeated, and are only recorded when they change.
#'
#' Missing values are replaced in atomic vectors; `NULL`s are replaced in lists.
#'
#' @param data A `data.frame`.
#' @param ... Columns to fill.
#' @param .direction Direction in which to fill missing values. Currently either `"down"` (the default), `"up"`,
#' `"downup"` (i.e. first down and then up) or `"updown"` (first up and then down).
#'
#' @examples
#' # Value (year) is recorded only when it changes
#' sales <- data.frame(
#'   quarter = c(
#'     "Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2",
#'     "Q3", "Q4", "Q1", "Q2", "Q3", "Q4"
#'   ),
#'   year = c(2000, NA, NA, NA, 2001, NA, NA, NA, 2002, NA, NA, NA, 2004, NA, NA, NA),
#'   sales = c(
#'     66013, 69182, 53175, 21001, 46036, 58842, 44568, 50197, 39113, 41668, 30144,
#'     52897, 32129, 67686, 31768, 49094
#'   )
#' )
#'
#' # `fill()` defaults to replacing missing data from top to bottom
#' sales %>% fill(year)
#'
#' # Value (pet_type) is missing above
#' tidy_pets <- data.frame(
#'   rank = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L),
#'   pet_type = c(NA, NA, NA, NA, NA, "Dog", NA, NA, NA, NA, NA, "Cat"),
#'   breed = c(
#'     "Boston Terrier", "Retrievers (Labrador)", "Retrievers (Golden)",
#'     "French Bulldogs", "Bulldogs", "Beagles", "Persian", "Maine Coon",
#'     "Ragdoll", "Exotic", "Siamese", "American Short"
#'   )
#' )
#'
#' # For values that are missing above you can use `.direction = "up"`
#' tidy_pets %>%
#'   fill(pet_type, .direction = "up")
#'
#' # Value (n_squirrels) is missing above and below within a group
#' squirrels <- data.frame(
#'   group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'   name = c(
#'     "Sam", "Mara", "Jesse", "Tom", "Mike", "Rachael", "Sydekea",
#'     "Gabriela", "Derrick", "Kara", "Emily", "Danielle"
#'   ),
#'   role = c(
#'     "Observer", "Scorekeeper", "Observer", "Observer", "Observer",
#'     "Observer", "Scorekeeper", "Observer", "Observer", "Scorekeeper",
#'     "Observer", "Observer"
#'   ),
#'   n_squirrels = c(NA, 8, NA, NA, NA, NA, 14, NA, NA, 9, NA, NA)
#' )
#'
#' # The values are inconsistently missing by position within the group
#' # Use .direction = "downup" to fill missing values in both directions
#' squirrels %>%
#'   group_by(group) %>%
#'   fill(n_squirrels, .direction = "downup") %>%
#'   ungroup()
#'
#' # Using `.direction = "updown"` accomplishes the same goal in this example
#'
#' @name fill
#' @export
fill <- function(data, ..., .direction = c("down", "up", "downup", "updown")) {
  UseMethod("fill")
}

#' @export
fill.data.frame <- function(data, ..., .direction = c("down", "up", "downup", "updown")) {
  col_pos <- select_positions(data, ...)
  if (length(col_pos) == 0L) return(data)
  .direction <- match.arg(.direction)
  # "downup" fills down and then fills what is left up, rather than filling each missing value from its nearest value
  steps <- switch(
    .direction,
    "up" = "up",
    "down" = "down",
    "updown" = c("up", "down"),
    "downup" = c("down", "up")
  )
  # Each group is filled on its own, so the rows are put into group order and filled in one pass
  rows <- group_layout(data)$rows
  idx <- unlist(rows, use.names = FALSE)
  first <- logical(length(idx))
  first[cumsum(c(1L, lengths(rows)[-length(rows)]))[lengths(rows) > 0L]] <- TRUE
  for (i in unname(col_pos)) {
    x <- .subset2(data, i)[idx]
    for (step in steps) x <- fill_values(x, first, up = step == "up")
    data[[i]][idx] <- x
  }
  data
}

#' Fill the missing values of a vector with the previous value in its group
#'
#' @param x A vector, in group order. Missing values are `NA` in atomic vectors and `NULL` in lists.
#' @param first `logical(n)`. Whether each element of `x` is the first of its group.
#' @param up `logical(1)`. Fill with the next value instead?
#' @return `x`, with each missing value replaced by the nearest earlier (or later) value in its group, if there is one.
#' @noRd
fill_values <- function(x, first, up) {
  missing <- if (is.list(x)) vapply(x, is.null, NA) else is.na(x)
  if (!any(missing)) return(x)
  n <- length(x)
  if (up) {
    # The first element of a group in reverse order is the last element of the group
    rev_idx <- rev(seq_len(n))
    from <- rev_idx[fill_from(missing[rev_idx], c(first[-1L], TRUE)[rev_idx])]
    return(x[from[rev_idx]])
  }
  x[fill_from(missing, first)]
}

#' Find the position of the value each element is filled from
#'
#' @param missing `logical(n)`. Whether each element is missing.
#' @param first `logical(n)`. Whether each element is the first of its group.
#' @return `integer(n)`. The position of the last element up to each element, within its group, which is not missing,
#' or of the first element of the group if there is none.
#' @noRd
fill_from <- function(missing, first) {
  cummax(seq_along(missing) * (!missing | first))
}
