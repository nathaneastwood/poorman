replace_with <- function(x, i, val, arg_name) {
  if (is.null(val)) return(x)
  check_length(val, x, arg_name)
  check_type(val, x, arg_name)
  check_class(val, x, arg_name)
  i[is.na(i)] <- FALSE
  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }
  x
}

check_length <- function(x, y, arg_name) {
  length_x <- length(x)
  length_y <- length(y)
  if (all(length_x %in% c(1L, length_y))) return()
  if (length_y == 1) {
    stop(arg_name, " must be length 1, not ", paste(length_x, sep = ", "))
  } else {
    stop(arg_name, " must be length ", length_y, " or 1, not ", length_x)
  }
}

check_type <- function(x, y, arg_name) {
  x_type <- typeof(x)
  y_type <- typeof(y)
  if (identical(x_type, y_type)) return()
  stop(arg_name, " must be `", y_type, "`, not `", x_type, "`")
}

check_class <- function(x, y, arg_name) {
  if (!is.object(x)) return()
  exp_classes <- class(y)
  out_classes <- class(x)
  if (identical(out_classes, exp_classes)) return()
  stop(arg_name, " must have class `", exp_classes, "`, not class `", out_classes, "`")
}
