#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are `TRUE`.
#'
#' @param .data A `data.frame`.
#' @param ... Logical predicated defined in terms of the variables in `.data`. Multiple conditions are combined with
#' `&`. Arguments within `...` are automatically quoted and evaluated within the context of the `data.frame`.
#' @param .preserve `logical(1)`. Relevant when the .data input is grouped. If `.preserve = FALSE` (the default), the
#' grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#'
#' @section Useful filter functions:
#'
#' * `==`, `>`, `>=`, etc.
#' * `&`, `|`, `!`, `xor()`
#' * `is.na()`
#'
#' @examples
#' filter(mtcars, am == 1)
#' mtcars %>% filter(cyl == 4)
#' mtcars %>% filter(cyl <= 5 & am > 0)
#' mtcars %>% filter(cyl == 4 | cyl == 8)
#' mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0)
#'
#' @return
#' A `data.frame`.
#'
#' @export
filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  conditions <- dotdotdot(...)
  if (length(conditions) == 0L) return(.data)
  check_filter(conditions)
  cond_class <- vapply(conditions, typeof, NA_character_)
  cond_class <- cond_class[!cond_class %in% c("language", "logical")]
  if (length(cond_class) > 0L) stop("Conditions must be logical vectors")
  context$setup(.data)
  on.exit(context$clean(), add = TRUE)
  eval_env$env <- parent.frame()
  on.exit(rm(list = "env", envir = eval_env), add = TRUE)
  rows <- lapply(
    conditions,
    function(cond, frame) eval(cond, context$.data, frame),
    frame = eval_env$env
  )
  rows <- Reduce("&", rows)
  .data[rows & !is.na(rows), ]
}

#' @export
filter.grouped_df <- function(.data, ..., .preserve = FALSE) {
  rows <- rownames(.data)
  res <- apply_grouped_function("filter", .data, drop = TRUE, ...)
  res <- res[rows[rows %in% rownames(res)], ]

  groups <- group_vars(.data)
  pre_filtered_groups <- group_data(.data)
  post_filtered_groups <- calculate_groups(res, groups)

  if (!(!.preserve && isTRUE(attr(pre_filtered_groups, ".drop")))) {
    filtered_groups <- anti_join(pre_filtered_groups, post_filtered_groups, by = groups)
    filtered_groups <- filtered_groups[, groups, drop = FALSE]
    filtered_groups[[".rows"]] <- rep(list(integer()), length.out = nrow(filtered_groups))
    post_filtered_groups <- bind_rows(post_filtered_groups, filtered_groups)
    ordered <- do.call(arrange_rows, list(post_filtered_groups, as_symbols(groups)))
    post_filtered_groups <- post_filtered_groups[ordered, ]
  }

  attr(res, "groups") <- post_filtered_groups
  res
}

# -- Helpers -------------------------------------------------------------------

check_filter <- function(conditions) {
  named <- have_name(conditions)
  for (i in which(named)) {
    if (!is.logical(conditions[[i]])) {
      stop(
        sprintf("Problem with `filter()` input `..%s`.\n", i),
        sprintf("Input `..%s` is named.\n", i),
        "This usually means that you've used `=` instead of `==`.\n",
        sprintf("Did you mean `%s == %s`?", names(conditions)[[i]], conditions[[i]])
      )
    }
  }
}
