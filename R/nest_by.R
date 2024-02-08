#' Nest By
#'
#' `nest_by()` is similar to [group_by()] however instead of storing the group structure in the metadata, it is made
#' explicit in the data. Each group key is given a single row within the `data.frame` and the group's data is stored
#' within a list-column of the `data.frame`.
#'
#' Currently there is no pretty-printing provided for the results of `nest_by()` and they are not useable with other
#' functions such as [mutate()].
#'
#' @inheritParams group_split
#' @param .key `character(1)`. The name of the column in which to nest the data (default: "data").
#'
#' @examples
#' mtcars %>% nest_by(am, cyl)
#' # Or equivalently
#' mtcars %>% group_by(am, cyl) %>% nest_by()
#'
#' @export
nest_by <- function(.data, ..., .key = "data", .keep = FALSE) {
  if (length(.key) != 1L || !is.character(.key)) stop("`.key` should be `character(1)`")
  if (length(.keep) != 1L || !is.logical(.keep)) stop("`.keep` should be `logical(1)`")
  UseMethod("nest_by")
}

#' @export
nest_by.data.frame <- function(.data, ..., .key = "data", .keep = FALSE) {
  .data <- group_by(.data, ...)
  nest_by(.data, .key = .key, .keep = .keep)
}

#' @export
nest_by.grouped_df <- function(.data, ..., .key = "data", .keep = FALSE) {
  if (!missing(...)) {
    stop("Can't re-group while nesting. Either `ungroup()` first or don't supply arguments to `nest_by()`")
  }
  nests <- group_split(.data, ..., .keep = TRUE)
  groups <- group_vars(.data)
  res <- lapply(
    nests,
    function(x) {
      df <- x[1, groups, drop = FALSE]
      df[[.key]] <- list(x[, !colnames(x) %in% groups, drop = FALSE])
      df
    }
  )
  res <- do.call(rbind, res)
  rownames(res) <- NULL
  res <- do.call(arrange, list(res, as.symbol(groups)))
  do.call(group_by, list(res, as.symbol(groups)))
}
