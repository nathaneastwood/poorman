#' Pivot data from long to wide
#'
#' `pivot_wider()` "widens" data, increasing the number of columns and decreasing
#'  the number of rows. The inverse transformation is `pivot_longer()`.
#' `tidyr::pivot_wider()`.
#'
#' @param data A data frame to pivot.
#' @param id_cols The name of the column that identifies the rows. If `NULL`,
#'   it will use all the unique rows.
#' @param names_from The name of the column that contains the levels to be
#'   used as future column names.
#' @param names_prefix String added to the start of every variable name. This is
#'  particularly useful if `names_from` is a numeric vector and you want to create
#'  syntactic variable names.
#' @param names_sep If `names_from` or `values_from` contains multiple variables,
#'   this will be used to join their values together into a single string to use
#'   as a column name.
#' @param names_glue Instead of `names_sep` and `names_prefix`, you can supply a
#'   [glue specification](https://glue.tidyverse.org/index.html) that uses the
#'   `names_from` columns to create custom column names. Note that the only
#'   delimiters supported by `names_glue` are curly brackets, `{` and `}`.
#' @param values_from The name of the column that contains the values to be used
#'   as future variable values.
#' @param values_fill Optionally, a (scalar) value that will be used to replace
#'   missing values in the new columns created.
#' @param ... Not used for now.
#'
#' @return If a tibble was provided as input, `pivot_wider()` also returns a
#' tibble. Otherwise, it returns a data frame.
#'
#' @examples
#' data_long <- read.table(header = TRUE, text = "
#'  subject sex condition measurement
#'        1   M   control         7.9
#'        1   M     cond1        12.3
#'        1   M     cond2        10.7
#'        2   F   control         6.3
#'        2   F     cond1        10.6
#'        2   F     cond2        11.1
#'        3   F   control         9.5
#'        3   F     cond1        13.1
#'        3   F     cond2        13.8
#'        4   M   control        11.5
#'        4   M     cond1        13.4
#'        4   M     cond2        12.9")
#'
#'
#' pivot_wider(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement"
#' )
#'
#' pivot_wider(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement",
#'   names_prefix = "Var.",
#'   names_sep = "."
#' )
#'
#' production <- expand.grid(
#'   product = c("A", "B"),
#'   country = c("AI", "EI"),
#'   year = 2000:2014
#' )
#' production <- filter(production, (product == "A" & country == "AI") | product == "B")
#'
#' production$production <- rnorm(nrow(production))
#'
#' pivot_wider(
#'   production,
#'   names_from = c("product", "country"),
#'   values_from = "production",
#'   names_glue = "prod_{product}_{country}"
#' )
#'
#' @export

pivot_wider <- function(
  data,
  id_cols = NULL,
  values_from = "Value",
  names_from = "Name",
  names_sep = "_",
  names_prefix = "",
  names_glue = NULL,
  values_fill = NULL,
  ...
) {

  old_names <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data)
  } else {
    tbl_input <- FALSE
  }
  variable_attr <- lapply(data, attributes)


  # Create an id for stats::reshape
  if (is.null(id_cols)) {
    data[["_Rows"]] <- apply(data[, !names(data) %in% c(values_from, names_from), drop = FALSE], 1, paste, collapse = "_")
    id_cols <- "_Rows"
  }


  # create pattern of column names - stats::reshape renames columns that
  # concatenates "v.names" + values - we only want values
  current_colnames <- colnames(data)
  current_colnames <- current_colnames[current_colnames != "_Rows"]
  if (is.null(names_glue)) {
    future_colnames <- unique(apply(data, 1, function(x) paste(x[c(names_from)], collapse = names_sep)))
  } else {
    vars <- regmatches(names_glue, gregexpr("\\{\\K[^{}]+(?=\\})", names_glue, perl = TRUE))[[1]]
    tmp_data <- unique(data[, vars])
    future_colnames <- unique(apply(tmp_data, 1, function(x) {
      tmp_vars <- list()
      for (i in seq_along(vars)) {
        tmp_vars[[i]] <- x[vars[i]]
      }

      tmp_colname <- gsub("\\{\\K[^{}]+(?=\\})", "", names_glue, perl = TRUE)
      tmp_colname <- gsub("\\{\\}", "%s", tmp_colname)
      do.call(sprintf, c(fmt = tmp_colname, tmp_vars))
    }))
  }


  # stop if some column names would be duplicated (follow tidyr workflow)
  if (any(future_colnames %in% current_colnames)) {
    stop(
      paste0(
        "Some values of the columns specified in 'names_from' are already present
        as column names. Either use `name_prefix` or rename the following columns: ",
        paste(current_colnames[which(current_colnames %in% future_colnames)], sep = ", ")
    ), call. = FALSE)
  }

  # stats::reshape works strangely when several variables are in idvar/timevar
  # so we unite all ids in a single temporary column that will be used by
  # stats::reshape
  data$new_time <- apply(data, 1, function(x) paste(x[names_from], collapse = "_"))
  data[, names_from] <- NULL

  wide <- stats::reshape(
    data,
    v.names = values_from,
    idvar = id_cols,
    timevar = "new_time",
    sep = names_sep,
    direction = "wide"
  )

  # Clean
  if ("_Rows" %in% names(wide)) wide[["_Rows"]] <- NULL
  row.names(wide) <- NULL # Reset row names

  if (length(values_from) == 1) {
    to_rename <- which(startsWith(names(wide), paste0(values_from, names_sep)))
    names(wide)[to_rename] <- future_colnames
  }

  # Order columns as in tidyr
  if (length(values_from) > 1) {
    for (i in values_from) {
      tmp1 <- wide[, which(!startsWith(names(wide), i))]
      tmp2 <- wide[, which(startsWith(names(wide), i))]
      wide <- cbind(tmp1, tmp2)
      # doesn't work
      # wide <- relocate(wide, starts_with(i), .after = -1)
    }
  }


  new_cols <- setdiff(names(wide), old_names)
  names(wide)[which(names(wide) %in% new_cols)] <- paste0(names_prefix, new_cols)

  # Fill missing values
  if (!is.null(values_fill)) {
    if (length(values_fill) == 1) {
      if (is.numeric(wide[[new_cols[1]]])) {
        if (!is.numeric(values_fill)) {
          stop(paste0("`values_fill` must be of type numeric."), call. = FALSE)
        } else {
          for (i in new_cols) {
            wide[[i]] <- replace_na(wide[[i]], replace = values_fill)
          }
        }
      } else if (is.character(wide[[new_cols[1]]])) {
        if (!is.character(values_fill)) {
          stop(paste0("`values_fill` must be of type character."), call. = FALSE)
        } else {
          for (i in new_cols) {
            wide[[i]] <- replace_na(wide[[i]], replace = values_fill)
          }
        }
      } else if (is.factor(wide[[new_cols[1]]])) {
        if (!is.factor(values_fill)) {
          stop(paste0("`values_fill` must be of type factor."), call. = FALSE)
        } else {
          for (i in new_cols) {
            wide[[i]] <- replace_na(wide[[i]], replace = values_fill)
          }
        }
      }
    } else {
      if (verbose) {
        stop("`values_fill` must be of length 1.", call. = FALSE)
      }
    }
  }


  # Remove reshape attributes
  attributes(wide)$reshapeWide <- NULL

  # add back attributes where possible
  for (i in colnames(wide)) {
    attributes(wide[[i]]) <- variable_attr[[i]]
  }

  if (isTRUE(tbl_input)) {
    class(wide) <- c("tbl_df", "tbl", "data.frame")
  }

  wide
}
