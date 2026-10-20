expect_equal(lst(NULL), list("NULL" = NULL))
expect_identical(lst(a = NULL), list(a = NULL))
expect_identical(
  lst(NULL, b = NULL, 1:3),
  list("NULL" = NULL, b = NULL, "1:3" = 1:3)
)

expect_identical(lst(a = 1, b = a), list(a = 1, b = 1))
expect_identical(lst(a = NULL, b = a), list(a = NULL, b = NULL))

expect_identical(lst(a = 1, a = a + 1, b = a), list(a = 1, a = 2, b = 2))
expect_identical(lst(b = 1, a = b, a = b + 1, b = a), list(b = 1, a = 1, a = 2, b = 2))

lst_fn <- function() {
  y <- 1
  lst(y, z = y + 1)
}
expect_identical(lst_fn(), list(y = 1, z = 2), info = "lst() finds variables in the calling function")

lst_env <- new.env()
expect_identical(
  evalq({
    a <- 1:3
    b <- letters[4:6]
    lst(a, b)
  }, lst_env),
  list(a = 1:3, b = letters[4:6]),
  info = "lst() finds variables in a non global calling environment (#120)"
)

lst_fn2 <- function() {
  x <- 10
  lst(x = x + 1, y = x)
}
expect_identical(lst_fn2(), list(x = 11, y = 11), info = "the first element of lst() can refer to a variable of the same name")
