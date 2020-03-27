expect_equal(
  mtcars %>% rownames_to_column(),
  {
    res <- mtcars
    cols <- colnames(res)
    res[, "rowname"] <- rownames(res)
    rownames(res) <- NULL
    res[, c("rowname", cols)]
  },
  info = "Test rownames to column"
)

expect_error(
  mtcars %>% rownames_to_column("mpg")
)
