df <- data.frame(x = 1:10, y = 1:10)
gf <- group_by(df, x)
expect_identical(
  arrange(df),
  df,
  info = "empty arrange() returns input for data.frames"
)
expect_identical(
  arrange(gf),
  gf,
  info = "empty arrange() returns input for grouped_df"
)

df <- data.frame(a = numeric(0))
expect_equal(
  arrange(df, a),
  df,
  info = "arrange() can sort empty data frame"
)

df <- data.frame(x = c(2, 1, NA))
expect_equal(
  df %>% arrange(x) %>% pull(),
  c(1, 2, NA),
  info = "local arrange() sorts missing values to the end"
)
expect_equal(
  df %>% arrange(desc(x)) %>% pull(),
  c(2, 1, NA),
  info = "local arrange() sorts missing values to the end"
)

# -- column types --------------------------------------------------------------

df <- data.frame(x = 1:3, y = 3:1 + 2i)
expect_equal(
  arrange(df, y),
  data.frame(x = 3:1, y = 1:3 + 2i),
  info = "arrange() handles complex columns"
)

TestS4 <- suppressWarnings(setClass("TestS4", contains = "integer"))
setMethod(
  "[",
  'TestS4',
  function(x, i, ...) TestS4(unclass(x)[i, ...])
)
df <- data.frame(x = 1:3, y = TestS4(3:1))
expect_equal(
  arrange(df, y),
  data.frame(x = 3:1, y = TestS4(1:3)),
  info = "arrange handles S4 classes"
)
removeClass("TestS4")

# -- data ----------------------------------------------------------------------

df1 <- data.frame(x = 1:3, y = 3:1)
df2 <- df1 %>% group_by(x)
expect_true(
  inherits(arrange(df1, x), "data.frame"),
  info = "arrange() preserves input class for data.frames"
)
expect_true(
  inherits(arrange(df2, x), "grouped_df"),
  info = "arrange() preserves input class for grouped_dfs"
)

df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
res <- df %>% group_by(g) %>% arrange(x)
expect_true(
  inherits(res, "grouped_df"),
  info = "arrange() updates the grouping structure: 1"
)
expect_equal(
  group_rows(res),
  list(c(2L, 4L), c(1L, 3L)),
  info = "arrange() updates the grouping structure: 2"
)

expect_equal(
  mtcars %>% arrange(mpg),
  mtcars[order(mtcars$mpg), ],
  info = "Test ascending column arrangement"
)

expect_equal(
  mtcars %>% arrange(-mpg),
  mtcars[order(-mtcars$mpg), ],
  info = "Test descending column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, mpg),
  mtcars[order(mtcars$cyl, mtcars$mpg), ],
  info = "Test multiple column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, -mpg),
  mtcars[order(mtcars$cyl - mtcars$mpg), ],
  info = "Test multiple ascending and descending column arrangement"
)

# data masking
df <- data.frame(x = 1:4, y = 5:8)
expect_equal(
  arrange(df, -x * y),
  structure(list(x = 4:1, y = 8:5), row.names = 1:4, class = "data.frame"),
  info = "arrange() can add and arrange by new columns (#89)"
)

# Grouped Operations
df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
res <- df %>% group_by(g) %>% arrange(x)
expect_true(
  inherits(res, "grouped_df"),
  info = "arrange() keeps the grouping class"
)

expect_equal(
  group_rows(res),
  list(c(2L, 4L), c(1L, 3L)),
  info = "arrange() keeps the grouping structure"
)

# .by_group
df <- data.frame(g = c(2, 1, 2, 1), x = 4:1)
gf <- group_by(df, g)
expect_equal(
  arrange(gf, x),
  structure(
    list(g = c(1, 2, 1, 2), x = 1:4),
    groups = structure(
      list(g = c(1, 2), .rows = list(c(1L, 3L), c(2L, 4L))), row.names = 1:2, class = "data.frame", .drop = TRUE
    ),
    row.names = 1:4,
    class = c("grouped_df", "data.frame")
  ),
  info = "grouped arrange() ignores group_by groups"
)
expect_equal(
  arrange(gf, x, .by_group = TRUE),
  structure(
    list(g = c(1, 1, 2, 2), x = c(1L, 3L, 2L, 4L)),
    groups = structure(list(g = c(1, 2), .rows = list(1:2, 3:4)), row.names = 1:2, class = "data.frame", .drop = TRUE),
    row.names = 1:4, class = c("grouped_df", "data.frame")
  ),
  info = "grouped arrange() ignores group, unless requested with .by_group"
)

# with character columns
df <- data.frame(x = c("a", "b", "a", "b"),
                 y = c("c", "c", "c", "d"),
                 z = c(4, 2, 1, 3))
expect_equal(
  df %>% arrange(-x),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1))
)
expect_equal(
  df %>% arrange(-x, y),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1)) %>%
    structure(row.names = 1:4)
)
expect_equal(
  df %>% arrange(-x, y, z),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 1, 4)) %>%
    structure(row.names = 1:4)
)

expect_equal(
  df %>% arrange(desc(x)),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1))
)
expect_equal(
  df %>% arrange(desc(x), y),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1)) %>%
    structure(row.names = 1:4)
)
expect_equal(
  df %>% arrange(desc(x), y, z),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 1, 4)) %>%
    structure(row.names = 1:4)
)

# -- across() ------------------------------------------------------------------

df <- data.frame(x = c(1, 3, 2, 1), y = c(4, 3, 2, 1))
expect_identical(
  df %>% arrange(across()),
  df %>% arrange(x, y),
  info = "arrange() supports across() (#88): 1"
)
expect_identical(
  df %>% arrange(across(.fns = desc)),
  df %>% arrange(desc(x), desc(y)),
  info = "arrange() supports across() (#88): 2"
)
expect_identical(
  df %>% arrange(across(x)),
  df %>% arrange(x),
  info = "arrange() supports across() (#88): 3"
)
expect_identical(
  df %>% arrange(across(y)),
  df %>% arrange(y),
  info = "arrange() supports across() (#88): 4"
)
expect_identical(
  df %>% arrange(across(c(y, x))),
  df %>% arrange(y, x),
  info = "arrange() sorts by across() columns in the selected order"
)
expect_identical(
  df %>% arrange(x, across(y, desc)),
  df %>% arrange(x, desc(y)),
  info = "arrange() can mix across() with other expressions"
)
expect_identical(
  df %>% arrange(across(where(is.numeric), ~ -.x)),
  df %>% arrange(desc(x), desc(y)),
  info = "arrange() supports across() with lambdas"
)
expect_identical(
  df %>% arrange(across(x, list(a = desc, b = identity))),
  df %>% arrange(desc(x)),
  info = "arrange() supports across() with a list of functions"
)
expect_identical(
  df %>% arrange(across(starts_with("z"))),
  df,
  info = "arrange() ignores across() when it selects no columns"
)
expect_identical(
  df[0, ] %>% arrange(across()),
  df[0, ],
  info = "arrange() supports across() on empty data frames"
)

df <- data.frame(a = c("b", "a", "c"), b = c(1, 2, 3))
expect_identical(
  df %>% arrange(across(a, desc)),
  df[c(3, 1, 2), ] %>% structure(row.names = 1:3),
  info = "arrange() supports across() with desc() on character columns"
)

df <- data.frame(g = c(2, 1, 2, 1), x = 4:1, y = c(1, 1, 2, 2))
gf <- group_by(df, g)
expect_equal(
  gf %>% arrange(across()),
  gf %>% arrange(g, x, y),
  info = "arrange() evaluates across() on the ungrouped data"
)

# -- expressions ---------------------------------------------------------------

df <- data.frame(x = c(2, 1, 3), y = c(1, 3, 2), z = c(3, 2, 1))
expect_identical(
  arrange(df, -x * y, z),
  df[3:1, ] %>% structure(row.names = 1:3),
  info = "arrange() supports compound expressions"
)
expect_identical(
  arrange(data.frame(x = c(1, 1, 2), y = c(1, 1, 0), a = c("p", "q", "r")), x + y, desc(a))$a,
  c("r", "q", "p"),
  info = "arrange() matches each expression to its own sort key"
)
expect_identical(
  arrange(df, desc(x) + 0),
  df[c(3, 1, 2), ] %>% structure(row.names = 1:3),
  info = "arrange() evaluates desc() when it is not the top level call"
)
expect_identical(
  arrange(df, desc(x + y)),
  df[c(3, 2, 1), ] %>% structure(row.names = 1:3),
  info = "arrange() supports desc() of an expression"
)
expect_identical(
  arrange(df, poorman::desc(y)),
  arrange(df, desc(y)),
  info = "arrange() supports namespaced desc()"
)
expect_identical(
  arrange(df, 1),
  df,
  info = "arrange() recycles size 1 expressions"
)
expect_identical(
  arrange(df, rev(x)),
  df[c(2, 3, 1), ] %>% structure(row.names = 1:3),
  info = "arrange() supports expressions without a direct column reference"
)
expect_identical(
  arrange(df, n():1),
  df[3:1, ] %>% structure(row.names = 1:3),
  info = "arrange() supports context functions"
)
expect_identical(
  arrange(df, desc(x + y + z + x + y + z + x + y + z + x + y + z + x + y + z + x + y + z + x + y + z + x)),
  arrange(df, desc(x)),
  info = "arrange() supports long expressions"
)
expect_error(
  arrange(df, 1:2),
  "must be size 3 or 1, not 2",
  info = "arrange() errors for expressions of the wrong size"
)
expect_error(
  arrange(df, NULL),
  info = "arrange() errors for NULL expressions"
)

f <- function(data) {
  w <- c(3, 1, 2)
  arrange(data, w)
}
expect_identical(
  f(df),
  df[c(2, 3, 1), ] %>% structure(row.names = 1:3),
  info = "arrange() finds variables in the calling environment"
)
f <- function(data, v) arrange(data, .data[[v]])
expect_identical(
  f(df, "y"),
  arrange(df, y),
  info = "arrange() supports the .data pronoun"
)

df <- data.frame(d = as.Date(c("2020-01-03", "2020-01-01", "2020-01-02")))
expect_identical(
  arrange(df, desc(d)),
  df[c(1, 3, 2), , drop = FALSE] %>% structure(row.names = 1:3),
  info = "arrange() supports desc() on Date columns"
)
