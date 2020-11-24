x <- list(1, 2, 3)
expect_equal(nth(x, 1), 1, info = "nth works with lists #1")
expect_equal(nth(x, 4), NULL, info = "nth works with lists #2")
expect_equal(nth(x, 4, default = 1), 1, info = "nth works with lists #3")

x <- 1:5
expect_equal(nth(x, -1), 5, info = "negative values index from end #1")
expect_equal(nth(x, -3), 3, info = "negative values index from end #2")

expect_equal(nth(1:4, 5), NA_integer_, info = "indexing past ends returns default value #1")
expect_equal(nth(1:4, -5), NA_integer_, info = "indexing past ends returns default value #2")
expect_equal(nth(1:4, -10), NA_integer_, info = "indexing past ends returns default value #3")

expect_equal(first(logical()), NA, info = "first() uses default value for 0 length logical vectors")
expect_equal(first(integer()), NA_integer_, info = "first() uses default value for 0 length integer vectors")
expect_equal(first(numeric()), NA_real_, info = "first() uses default value for 0 length numeric vectors")
expect_equal(first(character()), NA_character_, info = "first() uses default value for 0 length character vectors")
expect_equal(first(list()), NULL, info = "first() uses default value for 0 length lists")

fc <- factor("a")[0]
expect_equal(first(fc[0]), fc[NA], info = "first() uses default value for 0 length augmented vectors #1")
dt <- Sys.Date()
expect_equal(first(dt[0]), dt[NA], info = "first() uses default value for 0 length augmented vectors #2")
tm <- Sys.time()
expect_equal(first(tm[0]), tm[NA], info = "first() uses default value for 0 length augmented vectors #3")

expect_error(nth(1:10, "x"), info = "nth() gives meaningful error message")
