reconstruct_attrs <- function(data, template) {
  data <- remove_attributes(data)
  return(reconstruct_attrs_dispatch(data, template))
  UseMethod(generic = "reconstruct_attrs", object = template)
}

reconstruct_attrs_dispatch <- function(data, template) {
  UseMethod("reconstruct_attrs", template)
}

reconstruct_attrs.data.frame <- function(data, template) {
  attrs <- attributes(template)
  attrs$names <- names(data)
  attrs$row.names <- .row_names_info(data, type = 0L)
  attributes(data) <- attrs
  data
}

reconstruct_attrs.grouped_df <- function(data, template) {
  group_vars <- intersect(group_vars(template), colnames(data))
  structure(
    groups_set(data, group_vars, drop = group_by_drop_default(template)),
    class = c("grouped_df", "data.frame")
  )
}

remove_attributes <- function(data) {
  attrs <- attributes(data)
  foreign <- which(!names(attrs) %in% c("names", "row.names", "class"))
  if (length(foreign) > 0L) {
    for (i in foreign) {
      attr(data, names(attrs)[i]) <- NULL
    }
  }
  as.data.frame(data)
}
