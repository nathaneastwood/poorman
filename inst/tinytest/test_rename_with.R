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
