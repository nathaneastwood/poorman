expect_equal(
  {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    sink(tmp)
    x <- glimpse(mtcars)
    sink(NULL)
    x
  },
  mtcars,
  info = "The object is returned invisibly"
)
