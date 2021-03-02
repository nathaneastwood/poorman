reconstruct_attrs <- poorman:::reconstruct_attrs
reconstruct_attrs.data.frame <- poorman:::reconstruct_attrs.data.frame

expect_identical(
  reconstruct_attrs(data.frame(), data.frame()),
  data.frame(),
  info = "classes are restored for data.frames"
)
expect_identical(
  reconstruct_attrs(data.frame(), structure(data.frame(), class = c("foo", "data.frame"))),
  structure(data.frame(), class = c("foo", "data.frame")),
  info = "classes are restored for additional classes"
)

expect_identical(
  reconstruct_attrs(
    structure(list(), .Names = character(0), row.names = c(NA, -1L), class = "data.frame"),
    structure(list(), class = "data.frame", row.names = integer(0), .Names = character(0), foo = 1)
  ),
  structure(list(), class = "data.frame", row.names = c(NA, -1L), .Names = character(0), foo = 1),
  info = "attributes of `template` are kept"
)

data <- data.frame(a = c(1, 2))
template <- data.frame()
x <- reconstruct_attrs(data, template)
expect <- data.frame(a = c(1, 2))
expect_identical(x, expect, info = "compact row names are retained")
expect_identical(
  .row_names_info(x, type = 0L),
  .row_names_info(expect, type = 0L),
  info = "Explicitly ensure internal row name structure is identical"
)

df <- structure(list(x = 1), class = c("tmp", "data.frame"), row.names = 1L, foo = "bar")
out <- poorman:::remove_attributes(df)
expect_identical(
  out,
  data.frame(x = 1),
  info = "remove_attributes() strips attributes before dispatch"
)

df <- structure(list(x = 1), class = c("tmp", "data.frame"), row.names = "a", foo = "bar")
out <- poorman:::remove_attributes(df)
expect_identical(
  out,
  data.frame(x = 1, row.names = "a"),
  info = "remove_attributes() strips attributes before dispatch"
)
