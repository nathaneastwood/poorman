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

  col_pos <- select_positions(data, ..., .group_pos = TRUE)
  if (length(col_pos) == 0) return(data)
  .direction <- match.arg(.direction)

  # If "updown", then we apply direction = "up" for all non-NA, and then
  # we apply direction = "down" for all non-NA (and similarly for "downup").
  # So it's not the same as apply "up" and "down" consecutively for each non-NA

  steps <- switch(
    .direction,
    "up" = "up",
    "down" = "down",
    "updown" = c("up", "down"),
    "downup" = c("down", "up")
  )

  names(col_pos) <- NULL

  for (i in col_pos) {

    for (k in steps) {

      # code adapted from https://stackoverflow.com/a/13810615/11598948

      if (all(is.na(data[[i]]))) next

      if (k == "up") data[[i]] <- rev(data[[i]])
      ind <- which(!is.na(data[[i]]))

      if (is.na(data[[i]][1])) {
        ind <- c(1, ind)
      }
      rep_times <- diff(c(ind, length(data[[i]]) + 1))
      data[[i]] <- rep(data[[i]][ind], times = rep_times)
      if (k == "up") data[[i]] <- rev(data[[i]])
    }
  }

  return(data)
}

#' @export
fill.grouped_df <- function(data, ...) {
  context$group_env <- parent.frame(n = 1)
  on.exit(rm(list = c("group_env"), envir = context), add = TRUE)
  rows <- rownames(data)
  res <- apply_grouped_function("fill", data, drop = TRUE, ...)
  res[rows, , drop = FALSE]
}
