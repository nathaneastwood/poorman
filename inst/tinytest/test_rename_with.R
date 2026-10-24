expect_equal(
  colnames(rename_with(mtcars, toupper, 1)),
  c("MPG", colnames(mtcars)[2:ncol(mtcars)]),
  info = "rename_with() can select columns"
)

expect_equal(
  colnames(rename_with(mtcars, toupper, mpg)),
  c("MPG", colnames(mtcars)[2:ncol(mtcars)]),
  info = "rename_with() can select columns"
)

expect_equal(
  colnames(rename_with(mtcars, gsub, 1, pattern = "p", replacement = "P")),
  c("mPg", colnames(mtcars)[2:ncol(mtcars)]),
  info = "rename_with() passes ... along"
)

expect_error(
  mtcars %>% rename_with(.fn = function(x) rep("X", ncol(mtcars))),
  info = "Must not return duplicate columns"
)

expect_equal(
  mtcars %>% group_by(am, cyl) %>% rename_with(toupper) %>% group_vars(),
  c("CYL", "AM"),
  info = "Group names are updated"
)

gdf <- group_by(data.frame(id = 1:2, g = c("a", "b"), v = 3:4), id)

expect_silent(
  res <- rename_with(gdf, toupper, g),
  info = "rename_with() does not add grouping variables to the selection"
)
expect_equal(
  list(colnames(res), group_vars(res)),
  list(c("id", "G", "v"), "id"),
  info = "rename_with() only renames the selected columns of grouped data"
)

res <- rename_with(gdf, toupper, -id)
expect_equal(
  list(colnames(res), group_vars(res)),
  list(c("id", "G", "V"), "id"),
  info = "rename_with() leaves a deselected grouping variable alone"
)

res <- rename_with(gdf, toupper, -g)
expect_equal(
  list(colnames(res), group_vars(res)),
  list(c("ID", "g", "V"), "ID"),
  info = "rename_with() renames a grouping variable kept by a negative selection"
)
