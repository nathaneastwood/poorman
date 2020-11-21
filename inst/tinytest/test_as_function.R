expect_equal(
  poorman:::as_function("mean"),
  mean,
  "character vectors can be converted to functions"
)

expect_equal(
  poorman:::as_function(~mean)(x),
  mean,
  info = "formulas can be converted to functions"
)

expect_equal(
  poorman:::as_function(mean),
  mean,
  info = "functions are returned as is"
)

## Errors

expect_error(
  poorman:::as_function(x ~ mean),
  info = "cannot convert a two-sided formula"
)

expect_error(
  poorman:::as_function(2),
  info = "cannot convert objects which aren't functions, formulas or strings"
)
