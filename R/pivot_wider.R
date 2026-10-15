#' Pivot data from long to wide
#'
#' `pivot_wider()` "widens" data, increasing the number of columns and decreasing the number of rows. The inverse
#' transformation is [pivot_longer()].
#'
#' @param data `data.frame`. The data to pivot.
#' @param id_cols <[`poor-select`][select_helpers]>. The columns that uniquely identify each observation. Defaults to
#' all columns in `data` except for the columns specified in `names_from` and `values_from`.
#' @param names_from <[`poor-select`][select_helpers]>. The column(s) that contain the levels to be used as future
#' column names.
#' @param names_prefix `character(1)`. String added to the start of every variable name. This is particularly useful if
#' `names_from` is a numeric vector and you want to create syntactic variable names.
#' @param names_sep `character(1)`. If `names_from` or `values_from` contains multiple variables, this will be used to
#' join their values together into a single string to use as a column name.
#' @param names_glue `character(1)`. Instead of `names_sep` and `names_prefix`, you can supply a
#' [glue specification](https://glue.tidyverse.org/index.html) that uses the `names_from` columns to create custom
#' column names. Note that the only delimiters supported by `names_glue` are curly brackets, `{` and `}`.
#' @param values_from <[`poor-select`][select_helpers]>. The column(s) that contain the values to be used as future
#' variable values. If the values are not uniquely identified by the `id_cols` and `names_from` columns, the output
#' will contain list-columns and a warning is given.
#' @param values_fill Optionally, a scalar value that will be used to fill in missing values, i.e. combinations of
#' `id_cols` and `names_from` which are not present in `data`. Explicit `NA`s in `values_from` are not replaced. This
#' can also be a named list, specifying a fill value for each of the `values_from` columns.
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
#' ) %>%
#'   filter((product == "A" & country == "AI") | product == "B") %>%
#'   mutate(production = rnorm(nrow(.)))
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

  names_from <- names(eval_select_pos(data, substitute(names_from)))
  values_from <- names(eval_select_pos(data, substitute(values_from)))
  spec_cols <- c(names_from, values_from)
  id_cols <- substitute(id_cols)
  id_cols <- if (is.null(id_cols)) names(data) else names(eval_select_pos(data, id_cols))
  id_cols <- setdiff(id_cols, spec_cols)

  if (!is.null(values_fill)) {
    if (!is.list(values_fill)) values_fill <- rep(list(values_fill), length(values_from))
    if (is.null(names(values_fill))) names(values_fill) <- values_from
    if (any(lengths(values_fill) != 1L)) stop("`values_fill` must be of length 1.", call. = FALSE)
  }

  # Output rows: the unique combinations of the id columns, in order of appearance
  n_data <- nrow(data)
  row_id <- vec_group_id(as.list(data)[id_cols], n = n_data)
  row_first <- match(seq_len(max(row_id, 0L)), row_id)
  n_row <- length(row_first)
  if (length(id_cols) == 0L) n_row <- 1L

  # Output columns: the unique combinations of the `names_from` columns, in order of appearance
  name_id <- vec_group_id(as.list(data)[names_from], n = n_data)
  name_first <- match(seq_len(max(name_id, 0L)), name_id)
  n_name <- length(name_first)
  name_keys <- lapply(as.list(data)[names_from], function(x) {
    x <- vec_slice(x, name_first)
    if (is.factor(x)) as.character(x) else x
  })

  new_names <- unlist(lapply(values_from, function(value) {
    if (!is.null(names_glue)) {
      gluestick(names_glue, src = c(name_keys, list(.value = rep(value, n_name))))
    } else if (length(values_from) == 1L) {
      paste0(names_prefix, do.call(paste, c(unname(name_keys), sep = names_sep)))
    } else {
      paste0(value, names_sep, names_prefix, do.call(paste, c(unname(name_keys), sep = names_sep)))
    }
  }), use.names = FALSE)
  if (n_name == 0L) new_names <- character(0)

  # stop if some column names would be duplicated (follow tidyr workflow)
  clashes <- c(intersect(new_names, id_cols), new_names[duplicated(new_names)])
  if (length(clashes) > 0L) {
    stop(
      paste0(
        "Some values of the columns specified in 'names_from' are already present
        as column names. Either use `name_prefix` or rename the following columns: ",
        paste(unique(clashes), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Position of each input row within the output, cell by cell, column by column
  cell <- (name_id - 1) * n_row + row_id
  duplicates <- anyDuplicated(cell) > 0L

  value_out <- lapply(values_from, function(value) {
    val <- data[[value]]
    if (duplicates) {
      warning(
        "Values in `", value, "` are not uniquely identified; output will contain list-cols.",
        call. = FALSE
      )
      cells <- split(seq_len(n_data), cell)
      out <- vector("list", n_row * n_name)
      out[as.numeric(names(cells))] <- lapply(cells, function(i) vec_slice(val, i))
      return(lapply(seq_len(n_name), function(j) out[(j - 1) * n_row + seq_len(n_row)]))
    }
    pos <- rep(NA_integer_, n_row * n_name)
    pos[cell] <- seq_len(n_data)
    fill <- if (is.null(values_fill)) NULL else cast_fill(values_fill[[value]], val)
    lapply(seq_len(n_name), function(j) {
      idx <- pos[(j - 1) * n_row + seq_len(n_row)]
      col <- vec_slice(val, idx)
      if (!is.null(fill)) {
        if (is.integer(col) && is.double(fill)) col <- as.double(col)
        col[is.na(idx)] <- fill
      }
      col
    })
  })

  out <- c(
    lapply(as.list(data)[id_cols], vec_slice, row_first),
    unlist(value_out, recursive = FALSE)
  )
  names(out) <- c(id_cols, new_names)
  out <- structure(out, class = "data.frame", row.names = .set_row_names(n_row))
  reconstruct_attrs(out, data)
}

# Check that `values_fill` is compatible with the column of values it fills
cast_fill <- function(fill, val) {
  if (is.null(fill)) return(NULL)
  if (is.factor(val)) {
    if (!(is.factor(fill) || is.character(fill)) || !(as.character(fill) %in% levels(val))) {
      stop("`values_fill` must be of type factor.", call. = FALSE)
    }
    return(as.character(fill))
  }
  if (is.object(val)) {
    if (!inherits(fill, class(val)[1L])) stop("`values_fill` must be of class ", class(val)[1L], ".", call. = FALSE)
    return(fill)
  }
  if (is.numeric(val)) {
    if (!is.numeric(fill) || is.object(fill)) stop("`values_fill` must be of type numeric.", call. = FALSE)
    if (is.integer(val) && !is.na(fill) && fill == as.integer(fill)) fill <- as.integer(fill)
    return(fill)
  }
  if (is.character(val) && !is.character(fill)) stop("`values_fill` must be of type character.", call. = FALSE)
  if (is.logical(val)) {
    if (is.numeric(fill) && !is.object(fill) && fill %in% c(0, 1, NA)) fill <- as.logical(fill)
    if (!is.logical(fill)) stop("`values_fill` must be of type logical.", call. = FALSE)
  }
  fill
}
