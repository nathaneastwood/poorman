#' Pivot data from wide to long
#'
#' `pivot_longer()` "lengthens" data, increasing the number of rows and decreasing the number of columns. The inverse
#' transformation is [pivot_wider()].
#'
#' @param data `data.frame`. The data to pivot.
#' @param cols <[`poor-select`][select_helpers]>. Columns to pivot into longer format.
#' @param names_to `character(n)`. The name of the new column(s) that will contain the column names.
#' @param names_prefix `character(1)`. A regular expression used to remove matching text from the start of each variable
#' name.
#' @param names_sep,names_pattern `character(1)`. If `names_to` contains multiple values, this argument controls how the
#' column name is broken up. `names_pattern` takes a regular expression containing matching groups (`()`).
#' @param values_to `character(n)`. The name of the new column(s) that will contain the values of the pivoted variables.
#' @param values_drop_na `logical(1)`. If `TRUE`, will drop rows that contain only `NA` in the `values_to` column. This
#' effectively converts explicit missing values to implicit missing values, and should generally be used only when
#' missing values in data were created by its structure.
#' @param ... Additional arguments passed on to methods.
#'
#' @return A `data.frame`.
#'
#' @examples
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#' # Customizing the names
#' pivot_longer(
#'   data = wide_data,
#'   cols = c(1, 2),
#'   names_to = "Column",
#'   values_to = "Numbers"
#' )
#'
#' @export
pivot_longer <- function(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  values_to = "value",
  values_drop_na = FALSE,
  ...
) {

  if (missing(cols)) {
    stop("`cols` must select at least one column.")
  }

  cols <- names(eval_select_pos(data, substitute(cols)))

  if (any(names_to %in% setdiff(names(data), cols))) {
    stop(
      paste0(
        "Some values of the columns specified in 'names_to' are already present
        as column names. Either use another value in `names_to` or rename the
        following columns: ",
        paste(names_to[which(names_to %in% setdiff(names(data), cols))], sep = ", ")
      ),
      call. = FALSE)
  }

  # Sanity checks ----------------

  # nothing to select?
  if (length(cols) == 0L) {
    stop("No columns found for reshaping data.", call. = FALSE)
  }

  # Reshaping ---------------------
  # Create Index column as needed by reshape
  data[["_Row"]] <- as.numeric(rownames(data))

  # Create a new index for cases with length(names_to) > 1
  names_to_2 <- paste(names_to, collapse = "_")

  # Reshape
  long <- stats::reshape(
    data,
    varying = cols,
    idvar = "_Row",
    v.names = values_to,
    timevar = names_to_2,
    direction = "long"
  )

  # Cleaning --------------------------
  # Sort the dataframe (to match pivot_longer's output)
  long <- long[do.call(order, long[, c("_Row", names_to_2)]), ]

  long[["_Row"]] <- NULL

  # Re-insert col names as levels
  long[[names_to_2]] <- cols[long[[names_to_2]]]

  # if several variable in names_to, split the names either with names_sep or with names_pattern
  if (length(names_to) > 1) {
    for (i in seq_along(names_to)) {
      if (is.null(names_pattern)) {
        new_vals <- unlist(lapply(
          strsplit(unique(long[[names_to_2]]), names_sep, fixed = TRUE),
          function(x) x[i]
        ))
        long[[names_to[i]]] <- new_vals
      } else {
        colPattern <- regmatches(
          x = unique(long[[names_to_2]]),
          m = regexec(names_pattern, unique(long[[names_to_2]]))
        )
        colPattern <- as.data.frame(do.call(rbind, colPattern))[, c(1, i + 1)]
        names(colPattern) <- c(names_to_2, names_to[i])
        long <- left_join(x = long, y = colPattern, by = names_to_2)
      }
    }
    long[[names_to_2]] <- NULL
  }

  # reorder
  long <- relocate(.data = long, values_to, .after = -1)

  # remove names prefix if specified
  if (!is.null(names_prefix)) {
    if (length(names_to) > 1) {
      stop("`names_prefix` only works when `names_to` is of length 1.", call. = FALSE)
    }
    long[[names_to]] <- gsub(paste0("^", names_prefix), "", long[[names_to]])
  }

  if (values_drop_na) {
    long <- long[!is.na(long[, values_to]), ]
  }

  # Reset row names
  rownames(long) <- NULL

  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL

  long
}
