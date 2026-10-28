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
#' gluestick specification that uses the `names_from` columns to create custom column names. Note that the only
#' delimiters supported by `names_glue` are curly brackets, `{` and `}`.
#' @param values_from <[`poor-select`][select_helpers]>. The column(s) that contain the values to be used as future
#' variable values. If the values are not uniquely identified by the `id_cols` and `names_from` columns, the output
#' will contain list-columns and a warning is given.
#' @param names_sort `logical(1)`. Should the column names be sorted? If `FALSE`, the default, column names are ordered
#' by first appearance. Factors are sorted by their levels and characters in the C locale, with `NA` last.
#' @param names_repair `character(1)` or `function`. What happens if the output has invalid column names. The default,
#' `"check_unique"`, is to error if the columns are duplicated. Use `"minimal"` to allow duplicates in the output,
#' `"unique"` to de-duplicate by adding numeric suffixes or `"universal"` to also make the names syntactic. A function
#' is applied to the names.
#' @param values_fn `function` or named `list()`. Optionally, a function applied to the value in each cell in the
#' output, typically to summarise values which are not uniquely identified, e.g. `values_fn = mean`. A named list
#' gives a function for each `values_from` column, where functions may also be given as formulas, e.g.
#' `list(value = ~ max(.x))`. Use `values_fn = list` to create list-columns without a warning.
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
  names_from = "Name",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = "Value",
  values_fill = NULL,
  values_fn = NULL,
  ...
) {

  names_from <- names(eval_select_pos(data, substitute(names_from)))
  values_from <- names(eval_select_pos(data, substitute(values_from)))
  spec_cols <- c(names_from, values_from)
  id_cols <- substitute(id_cols)
  id_cols <- if (is.null(id_cols)) names(data) else names(eval_select_pos(data, id_cols))
  id_cols <- setdiff(id_cols, spec_cols)

  if (is.function(values_fn)) values_fn <- structure(rep(list(values_fn), length(values_from)), names = values_from)
  if (!is.null(values_fn) && !is.list(values_fn)) {
    stop("`values_fn` must be a NULL, a function, or a named list", call. = FALSE)
  }
  if (!is.null(values_fill)) {
    is_scalar <- if (is.list(values_fill)) length(values_fill) == 1L && is.null(names(values_fill)) else TRUE
    if (is_scalar) values_fill <- structure(rep(list(values_fill), length(values_from)), names = values_from)
    if (any(lengths(values_fill) != 1L)) stop("`values_fill` must be of length 1.", call. = FALSE)
  }

  # Output rows: the unique combinations of the id columns, in order of appearance
  n_data <- nrow(data)
  row_id <- group_ids(as.list(data)[id_cols], n = n_data)
  row_first <- first_positions(row_id)
  n_row <- length(row_first)
  if (length(id_cols) == 0L) n_row <- 1L

  # Output columns: the unique combinations of the `names_from` columns, in order of appearance
  name_id <- group_ids(as.list(data)[names_from], n = n_data)
  name_first <- first_positions(name_id)
  n_name <- length(name_first)
  if (isTRUE(names_sort) && n_name > 1L) {
    sort_keys <- lapply(as.list(data)[names_from], function(x) {
      x <- vec_slice(x, name_first)
      if (is.factor(x)) as.integer(x) else unclass(x)
    })
    name_order <- do.call(order, c(unname(sort_keys), method = "radix"))
    name_first <- name_first[name_order]
    name_id <- match(name_id, name_order)
  }
  name_keys <- lapply(as.list(data)[names_from], function(x) {
    x <- vec_slice(x, name_first)
    if (is.factor(x)) as.character(x) else x
  })

  new_names <- unlist(lapply(values_from, function(value) {
    if (!is.null(names_glue)) {
      rep_len(gluestick(names_glue, src = c(name_keys, list(.value = rep(value, n_name)))), n_name)
    } else if (length(values_from) == 1L) {
      paste0(names_prefix, do.call(paste, c(unname(name_keys), sep = names_sep)))
    } else {
      paste0(value, names_sep, names_prefix, do.call(paste, c(unname(name_keys), sep = names_sep)))
    }
  }), use.names = FALSE)
  if (n_name == 0L) new_names <- character(0)

  # stop if some column names would be duplicated; the new names must always be unique
  clashes <- c(intersect(new_names, id_cols), new_names[duplicated(new_names)])
  if (anyDuplicated(new_names)) names_repair <- "check_unique"
  out_names <- repair_names(
    c(id_cols, new_names),
    names_repair,
    error_msg = paste0(
      "Some values of the columns specified in 'names_from' are already present
        as column names. Either use `name_prefix` or rename the following columns: ",
      paste(unique(clashes), collapse = ", ")
    )
  )

  # Position of each input row within the output, cell by cell, column by column
  cell <- (name_id - 1) * n_row + row_id
  n_cell <- n_row * n_name
  duplicates <- if (n_cell < .Machine$integer.max) {
    any(tabulate(as.integer(cell), n_cell) > 1L)
  } else {
    anyDuplicated(cell) > 0L
  }

  value_out <- lapply(values_from, function(value) {
    val <- data[[value]]
    fn <- values_fn[[value]]
    val_cell <- cell
    if (duplicates || !is.null(fn)) {
      if (is.null(fn)) {
        warning(
          "Values are not uniquely identified; output will contain list-cols.\n",
          "* Use `values_fn = list` to suppress this warning.\n",
          "* Use `values_fn = length` to identify where the duplicates arise\n",
          "* Use `values_fn = {summary_fun}` to summarise duplicates",
          call. = FALSE
        )
      }
      cells <- split(seq_len(n_data), cell)
      val_cell <- as.numeric(names(cells))
      val <- lapply(cells, function(i) vec_slice(val, i))
      if (!is.null(fn) && !identical(fn, list)) {
        fn <- as_function(fn)
        val <- lapply(val, fn)
        if (any(lengths(val) != 1L)) stop("`values_fn` must summarise each value to a single value.", call. = FALSE)
        val <- combine_columns(val)
      } else {
        names(val) <- NULL
      }
    }
    pos <- rep(NA_integer_, n_row * n_name)
    pos[val_cell] <- seq_along(val_cell)
    fill <- if (is.null(values_fill)) NULL else cast_fill(values_fill[[value]], val)
    lapply(seq_len(n_name), function(j) {
      idx <- pos[(j - 1) * n_row + seq_len(n_row)]
      col <- vec_slice(val, idx)
      if (!is.null(fill)) col[is.na(idx)] <- fill
      col
    })
  })

  out <- c(
    lapply(as.list(data)[id_cols], vec_slice, row_first),
    unlist(value_out, recursive = FALSE)
  )
  names(out) <- out_names
  out <- new_data_frame(out, n_row)
  reconstruct_attrs(out, data)
}

# Check that `values_fill` is compatible with the column of values it fills
cast_fill <- function(fill, val) {
  if (is.null(fill)) return(NULL)
  if (is.logical(fill) && !is.object(fill) && is.na(fill)) return(vec_slice(val, NA_integer_))
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
    if (is.integer(val) && is.double(fill)) {
      if (!is.na(fill) && fill != round(fill)) {
        stop("Can't convert `values_fill` <double> to <integer> due to loss of precision.", call. = FALSE)
      }
      fill <- as.integer(fill)
    }
    return(fill)
  }
  if (is.character(val) && !is.character(fill)) stop("`values_fill` must be of type character.", call. = FALSE)
  if (is.logical(val)) {
    if (is.numeric(fill) && !is.object(fill) && fill %in% c(0, 1, NA)) fill <- as.logical(fill)
    if (!is.logical(fill)) stop("`values_fill` must be of type logical.", call. = FALSE)
  }
  fill
}
