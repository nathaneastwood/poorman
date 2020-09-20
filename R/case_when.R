#' A General Vetorised `if()`
#'
#' This function allows you to vectorise multiple [if_else()] statements. It is an R equivalent of the SQL `CASE WHEN`
#' statement. If no cases match, `NA` is returned.
#'
#' @param ... A sequence of two-sided formulas. The left hand side (LHS) determines which values match this case. The
#' right hand side (RHS) provides the replacement value.
#'
#' The LHS must evaluate to a logical vector. The RHS does not need to be logical, but all RHSs must evaluate to the
#' same type of vector.
#'
#' Both LHS and RHS may have the same length of either 1 or n. The value of n must be consistent across all cases. The
#' case of `n == 0` is treated as a variant of `n != 1`.
#'
#' `NULL` inputs are ignored.
#'
#' @return
#' A vector of length 1 or n, matching the length of the logical input or output vectors, with the type (and attributes)
#' of the first RHS. Inconsistent lengths or types will generate an error.
#'
#' @examples
#' x <- 1:50
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#'
#' # Like an if statement, the arguments are evaluated in order, so you must
#' # proceed from the most specific to the most general. This won't work:
#' case_when(
#'   TRUE ~ as.character(x),
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz"
#' )
#'
#' # If none of the cases match, NA is used:
#' case_when(
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz"
#' )
#'
#' # Note that NA values in the vector x do not get special treatment. If you want
#' # to explicitly handle NA values you can use the `is.na` function:
#' x[2:4] <- NA_real_
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   is.na(x) ~ "nope",
#'   TRUE ~ as.character(x)
#' )
#'
#' # All RHS values need to be of the same type. Inconsistent types will throw an error.
#' # This applies also to NA values used in RHS: NA is logical, use
#' # typed values like NA_real_, NA_complex, NA_character_, NA_integer_ as appropriate.
#' case_when(
#'   x %% 35 == 0 ~ NA_character_,
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA_real_
#' )
#'
#' # case_when() evaluates all RHS expressions, and then constructs its
#' # result by extracting the selected (via the LHS expressions) parts.
#' # In particular NaN are produced in this case:
#' y <- seq(-2, 2, by = .5)
#' case_when(
#'   y >= 0 ~ sqrt(y),
#'   TRUE   ~ y
#' )
#'
#' \dontrun{
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA
#' )
#' }
#'
#' # case_when is particularly useful inside mutate when you want to
#' # create a new variable that relies on a complex combination of existing
#' # variables
#' mtcars %>%
#'   mutate(
#'     efficient = case_when(
#'       mpg > 25 ~ TRUE,
#'       TRUE ~ FALSE
#'     )
#'   )
#'
#' @export
case_when <- function (...) {
  fs <- list(...)
  lapply(fs, function(x) if (class(x) != "formula") stop("`case_when()` requires formula inputs."))
  n <- length(fs)
  if (n == 0L) stop("No cases provided.")
  query <- vector("list", n)
  value <- vector("list", n)
  default_env <- parent.frame()
  for (i in seq_len(n)) {
    query[[i]] <- eval(fs[[i]][[2]], envir = default_env)
    value[[i]] <- eval(fs[[i]][[3]], envir = default_env)
    if (!is.logical(query[[i]])) stop(fs[[i]][[2]], " does not return a `logical` vector.")
  }
  m <- validate_case_when_length(query, value, fs)
  out <- value[[1]][rep(NA_integer_, m)]
  replaced <- rep(FALSE, m)
  for (i in seq_len(n)) {
    out <- replace_with(out, query[[i]] & !replaced, value[[i]], NULL)
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }
  out
}

validate_case_when_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))
  if (length(all_lengths) <= 1L) return(all_lengths[[1L]])
  non_atomic_lengths <- all_lengths[all_lengths != 1L]
  len <- non_atomic_lengths[[1L]]
  if (length(non_atomic_lengths) == 1L) return(len)
  inconsistent_lengths <- non_atomic_lengths[-1L]
  lhs_problems <- lhs_lengths %in% inconsistent_lengths
  rhs_problems <- rhs_lengths %in% inconsistent_lengths
  problems <- lhs_problems | rhs_problems
  if (any(problems)) {
    stop(
      "The following formulas must be length ", len, " or 1, not ",
      paste(inconsistent_lengths, collapse = ", "), ".\n    ",
      paste(fs[problems], collapse = "\n    ")
    )
  }
}
