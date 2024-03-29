% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill.R
\name{fill}
\alias{fill}
\title{Fill in missing values with previous or next value}
\usage{
fill(data, ..., .direction = c("down", "up", "downup", "updown"))
}
\arguments{
\item{data}{A \code{data.frame}.}

\item{...}{Columns to fill.}

\item{.direction}{Direction in which to fill missing values. Currently either \code{"down"} (the default), \code{"up"},
\code{"downup"} (i.e. first down and then up) or \code{"updown"} (first up and then down).}
}
\description{
Fills missing values in selected columns using the next or previous entry. This is useful in the common output format
where values are not repeated, and are only recorded when they change.
}
\details{
Missing values are replaced in atomic vectors; \code{NULL}s are replaced in lists.
}
\examples{
# Value (year) is recorded only when it changes
sales <- data.frame(
  quarter = c(
    "Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2",
    "Q3", "Q4", "Q1", "Q2", "Q3", "Q4"
  ),
  year = c(2000, NA, NA, NA, 2001, NA, NA, NA, 2002, NA, NA, NA, 2004, NA, NA, NA),
  sales = c(
    66013, 69182, 53175, 21001, 46036, 58842, 44568, 50197, 39113, 41668, 30144,
    52897, 32129, 67686, 31768, 49094
  )
)

# `fill()` defaults to replacing missing data from top to bottom
sales \%>\% fill(year)

# Value (pet_type) is missing above
tidy_pets <- data.frame(
  rank = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L),
  pet_type = c(NA, NA, NA, NA, NA, "Dog", NA, NA, NA, NA, NA, "Cat"),
  breed = c(
    "Boston Terrier", "Retrievers (Labrador)", "Retrievers (Golden)",
    "French Bulldogs", "Bulldogs", "Beagles", "Persian", "Maine Coon",
    "Ragdoll", "Exotic", "Siamese", "American Short"
  )
)

# For values that are missing above you can use `.direction = "up"`
tidy_pets \%>\%
  fill(pet_type, .direction = "up")

# Value (n_squirrels) is missing above and below within a group
squirrels <- data.frame(
  group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  name = c(
    "Sam", "Mara", "Jesse", "Tom", "Mike", "Rachael", "Sydekea",
    "Gabriela", "Derrick", "Kara", "Emily", "Danielle"
  ),
  role = c(
    "Observer", "Scorekeeper", "Observer", "Observer", "Observer",
    "Observer", "Scorekeeper", "Observer", "Observer", "Scorekeeper",
    "Observer", "Observer"
  ),
  n_squirrels = c(NA, 8, NA, NA, NA, NA, 14, NA, NA, 9, NA, NA)
)

# The values are inconsistently missing by position within the group
# Use .direction = "downup" to fill missing values in both directions
squirrels \%>\%
  group_by(group) \%>\%
  fill(n_squirrels, .direction = "downup") \%>\%
  ungroup()

# Using `.direction = "updown"` accomplishes the same goal in this example

}
