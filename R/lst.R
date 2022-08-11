#' Build a list
#'
#' @description
#' `lst()` constructs a list, similar to [base::list()], but where components
#' are built sequentially. When defining a component, you can refer to components
#' created earlier in the call. `lst()` also generates missing names
#' automatically.
#'
#' @param ... Named or unnamed elements of a list. If the element is unnamed, its
#' expression will be used as its name.
#'
#' @return A named list.
#' @export
#' @examples
#' # the value of n can be used immediately in the definition of x
#' lst(n = 5, x = runif(n))
#'
#' # missing names are constructed from user's input
#' lst(1:3, z = letters[4:6], runif(3))
#'
#' a <- 1:3
#' b <- letters[4:6]
#' lst(a, b)

lst <- function(...) {
  fnCall <- match.call()
  listToEval <- as.list(fnCall)[-1]

  out <- vector(mode = "list", length = length(listToEval))
  names(out) <- names(listToEval)
  exprs <- lapply(substitute(list(...)), deparse)[-1]
  for (element in seq_along(listToEval)) {
    value <- listToEval[[element]]
    if (is.language(value)) {
      # need to update the environment in which the values are obtained
      # ex: lst(a = 1, a = a + 1, b = a), 'b' needs the updated value of 'a',
      # not its initial value.
      value <- eval(
        value,
        envir = if (length(out) == 0) {
          listToEval
        } else {
          # restrict the environment to the previous elements of the list (and
          # to the last value for each name if there are duplicated names)
          drop_dup_list(out[1:(element - 1)])
        }
      )
    }
    if (is.null(value)) {
      out[element] <- list(NULL)
    } else {
      out[[element]] <- value
    }

    # this naming part needs to happen at the end of the loop to avoid error
    # with lst(NULL)
    invalid_name <- is.null(names(out)[element]) ||
      is.na(names(out)[element]) ||
      names(out)[element] == ""

    if (invalid_name) {
      if (exprs[[element]] != "NULL" | (exprs[[element]] == "NULL" & is.null(out[[element]]))) {
        names(out)[element] <- exprs[[element]]
      }
    }
  }
  out
}

# if several elements of a list have the same name, only keep the last one
# with this name.
# Ex: list(a = 1, a = 2, b = 1) -> list(a = 2, b = 1)
drop_dup_list <- function(x) {

  list_names <- names(x)
  if (identical(list_names, unique(list_names))) return(x)

  count <- table(list_names)
  dupes <- names(count[count > 1])
  uniques <- names(count[count == 1])

  to_drop <- do.call(c, lapply(
    dupes,
    function(x) {
      matches <- which(list_names == x)
      matches[-length(matches)]
    }
  ))
  x[uniques] <- Filter(Negate(is.null), x[uniques])

  return(x[-to_drop])

}
