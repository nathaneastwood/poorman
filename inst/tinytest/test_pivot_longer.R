state_x77 <- as.data.frame(state.x77) %>% rownames_to_column(var = "State")

res <- state_x77 %>%
  pivot_longer(cols = -State, names_to = "Statistic", values_to = "Value")

expect_equal(dim(res), c(400, 3))
expect_equal(colnames(res), c("State", "Statistic", "Value"))

df <- data.frame(id = c(1, 2), v1.act = c(0.4, 0.8), v2.act = c(0.5, 0.7), v1.fix = c(1, 0), v2.fix = c(0, 1))
res <- pivot_longer(df, -id, names_to = c("var", "type"), names_pattern = "v(.).(.*)")
expect_equal(dim(res), c(8, 4))
expect_equal(colnames(res), c("id", "var", "type", "value"))

res <- pivot_longer(df, -id, names_to = c("var", "type"), names_sep = ".")
expect_equal(dim(res), c(8, 4))
expect_equal(colnames(res), c("id", "var", "type", "value"))

df <- data.frame(x = 1:2, y = 3:4)
pv <- pivot_longer(df, x:y)
expect_equal(names(pv), c("name", "value"))
expect_equal(pv$name, rep(names(df), 2))
expect_equal(pv$value, c(1, 3, 2, 4))


df <- data.frame(
  x = c(1, 2),
  y = c(10, 20),
  z = c(100, 200)
)
pv <- pivot_longer(df, 1:3)
expect_equal(pv$value, c(1, 10, 100, 2, 20, 200))


df <- data.frame(x = 1:2, y = 2, z = 1:2)
pv <- pivot_longer(df, y:z)
expect_equal(names(pv), c("x", "name", "value"))
expect_equal(pv$x, rep(df$x, each = 2))


df <- data.frame(x = c(1, NA), y = c(NA, 2))
pv <- pivot_longer(df, x:y, values_drop_na = TRUE)
expect_equal(pv$name, c("x", "y"))
expect_equal(pv$value, c(1, 2))


df <- data.frame(x = factor("a"), y = factor("b"))
pv <- pivot_longer(df, x:y)
expect_equal(pv$value, factor(c("a", "b")))


df <- data.frame(x = 1, y = 2)

expect_error(
  pivot_longer(df, y, names_to = "x"),
  pattern = "Some values of the columns specified in 'names_to' are already present"
)

# Expressions which evaluate to column names can be used to select columns (#118)
df <- data.frame(X = 1:2, Y1 = c(0.1, 0.2), Y2 = c(0.3, 0.4))
expected <- pivot_longer(df, c("Y1", "Y2"))
expect_equal(pivot_longer(df, names(df)[-1]), expected, info = "Expressions returning column names work")
get_y <- function() c("Y1", "Y2")
expect_equal(pivot_longer(df, get_y()), expected, info = "Function calls returning column names work")
local_pivot <- function(data) {
  nd <- names(data)[-1]
  pivot_longer(data, nd)
}
expect_equal(local_pivot(df), expected, info = "Variables local to the calling function can be used")
local_pivot_all_of <- function(data, cols) pivot_longer(data, all_of(cols))
expect_equal(local_pivot_all_of(df, c("Y1", "Y2")), expected, info = "all_of() finds local variables")
expect_equal(
  poorman::pivot_longer(df, poorman::starts_with("Y")), expected,
  info = "Namespaced selection helpers work"
)

# Data with non-numeric row names can be pivoted
res <- pivot_longer(mtcars[1:2, c("mpg", "cyl")], c(mpg, cyl))
expect_equal(
  res,
  data.frame(name = rep(c("mpg", "cyl"), 2), value = c(21, 6, 21, 6), stringsAsFactors = FALSE),
  info = "Character row names do not affect pivot_longer()"
)

# Rows are returned in the order of the input rather than by row name
df <- data.frame(id = 1:4, a = 1:4, b = 5:8)
res <- pivot_longer(df[c(4, 2, 3, 1), ], c(a, b))
expect_equal(res$id, rep(c(4L, 2L, 3L, 1L), each = 2), info = "Input row order is kept")
expect_equal(res$value, c(4L, 8L, 2L, 6L, 3L, 7L, 1L, 5L), info = "Values follow the input row order")
expect_equal(attr(res, "row.names"), 1:8, info = "Row names are reset")

# Zero row data can be pivoted
res <- pivot_longer(df[0, ], c(a, b))
expect_equal(
  res,
  data.frame(id = integer(), name = character(), value = integer(), stringsAsFactors = FALSE),
  info = "Zero row data returns a zero row data.frame"
)

# The class of the input is kept, e.g. for tibbles
tbl <- structure(df, class = c("tbl_df", "tbl", "data.frame"))
res <- pivot_longer(tbl, c(a, b))
expect_equal(class(res), c("tbl_df", "tbl", "data.frame"), info = "The input class is kept")
expect_equal(res$value, c(1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L), info = "Values of tibble-like input are correct")

# Grouped data stays grouped
res <- df %>% group_by(id) %>% pivot_longer(c(a, b))
expect_equal(group_vars(res), "id", info = "Grouping variables are kept")
expect_equal(nrow(group_data(res)), 4L, info = "Groups are recomputed")

# A single names_to column is extracted using the names_pattern group
df <- data.frame(id = 1, wk1 = 1, wk2 = 2)
res <- pivot_longer(df, -id, names_to = "week", names_pattern = "wk(.*)")
expect_equal(res$week, c("1", "2"), info = "names_pattern extracts a single group")

# names_prefix works with several names_to columns
df <- data.frame(id = 1, x_a_1 = 1, x_b_2 = 2)
res <- pivot_longer(df, -id, names_to = c("letter", "number"), names_prefix = "x_", names_sep = "_")
expect_equal(res$letter, c("a", "b"), info = "names_prefix is removed before splitting (1)")
expect_equal(res$number, c("1", "2"), info = "names_prefix is removed before splitting (2)")

# NA in names_to discards that piece
df <- data.frame(id = 1, a_x = 1, a_y = 2, b_x = 3)
res <- pivot_longer(df, -id, names_to = c("g", NA), names_sep = "_")
expect_equal(names(res), c("id", "g", "value"), info = "NA names_to pieces are dropped")
expect_equal(res$g, c("a", "a", "b"), info = "Columns with the same remaining name are not merged")
expect_equal(res$value, c(1, 2, 3), info = "No values are lost when dropping pieces")

# Missing pieces are filled with NA and warn
df <- data.frame(id = 1, a_x = 1, b = 2)
expect_warning(
  res <- pivot_longer(df, -id, names_to = c("p", "q"), names_sep = "_"),
  pattern = "Expected 2 pieces. Missing pieces filled with `NA` in 1 rows \\[2\\]"
)
expect_equal(res$q, c("x", NA), info = "Missing pieces are NA")

# names_to can contain ".value" to create several value columns
df <- data.frame(id = 1:2, x_1 = 1:2, y_2 = 3:4, y_1 = 5:6)
res <- pivot_longer(df, -id, names_to = c(".value", "n"), names_sep = "_")
expect_equal(
  res,
  data.frame(id = c(1L, 1L, 2L, 2L), n = c("1", "2", "1", "2"), x = c(1L, NA, 2L, NA), y = c(5L, 3L, 6L, 4L),
             stringsAsFactors = FALSE),
  info = ".value creates a column per value"
)
df <- data.frame(id = 1:2, x_1 = c(1, NA), x_2 = c(NA, NA), y_1 = c(NA, NA), y_2 = 3:4)
res <- pivot_longer(df, -id, names_to = c(".value", "n"), names_sep = "_", values_drop_na = TRUE)
expect_equal(res$n, c("1", "2", "2"), info = "values_drop_na drops rows where all values are missing")

# Values are combined following the tidyr type rules
df <- data.frame(a = 1:2, b = c(0.5, 1))
expect_equal(pivot_longer(df, a:b)$value, c(1, 0.5, 2, 1), info = "integer and double combine to double")
df <- data.frame(a = factor(c("x", "y")), b = c("z", "x"), stringsAsFactors = FALSE)
expect_equal(pivot_longer(df, a:b)$value, c("x", "z", "y", "x"), info = "factor and character combine to character")
df <- data.frame(a = factor(c("x", "y")), b = factor(c("z", "x")))
expect_equal(
  pivot_longer(df, a:b)$value, factor(c("x", "z", "y", "x"), levels = c("x", "y", "z")),
  info = "factors combine to a factor with the union of the levels"
)
df <- data.frame(a = c(NA, NA), b = c("z", "x"), stringsAsFactors = FALSE)
expect_equal(pivot_longer(df, a:b)$value, c(NA, "z", NA, "x"), info = "Missing columns combine with any type")
df <- data.frame(a = as.Date(c("2020-01-01", "2020-01-02")), b = as.Date(c("2021-01-01", "2021-01-02")))
expect_equal(
  pivot_longer(df, a:b)$value, as.Date(c("2020-01-01", "2021-01-01", "2020-01-02", "2021-01-02")),
  info = "Dates are kept"
)
df <- data.frame(a = 1:2, b = c("z", "x"), stringsAsFactors = FALSE)
expect_error(pivot_longer(df, a:b), pattern = "Can't combine `a` <integer> and `b` <character>")
df <- data.frame(a = as.Date("2020-01-01"), b = 1)
expect_error(pivot_longer(df, a:b), pattern = "Can't combine `a` <Date> and `b` <double>")

# Attributes of id columns are kept
df <- data.frame(id = 1:2, a = 1:2, b = 3:4)
attr(df$id, "label") <- "Identifier"
res <- pivot_longer(df, a:b)
expect_equal(attr(res$id, "label"), "Identifier", info = "Attributes of id columns are kept")

# Invalid inputs are caught
df <- data.frame(id = 1:2, a = 1:2, b = 3:4)
expect_error(pivot_longer(df, a:b, values_to = "id"), pattern = "must not be duplicated")
expect_error(pivot_longer(df, a:b, names_to = "x", values_to = "x"), pattern = "must not be duplicated")
expect_error(pivot_longer(df, a:b, names_to = c("x", "y")), pattern = "you must also supply one of")
expect_error(pivot_longer(df, a:b, names_sep = "_"), pattern = "can not be used with length-1 `names_to`")
expect_error(pivot_longer(df, a:b, values_to = c("x", "y")), pattern = "`values_to` must be a single string")
expect_error(
  pivot_longer(data.frame(id = 1, a_x = 1), -id, names_to = c("p", "q"), names_pattern = "(.)_x"),
  pattern = "should define 2 groups; 1 found"
)

# names_to = character() creates no name column
df <- data.frame(id = 1:2, x_1 = 1:2, x_2 = 3:4)
res <- pivot_longer(df, -id, names_to = character())
expect_equal(res, data.frame(id = c(1L, 1L, 2L, 2L), value = c(1L, 3L, 2L, 4L)), info = "No name column")

# Outputs which are not unique are told apart rather than lost
res <- pivot_longer(df, -id, names_to = c(".value", NA), names_sep = "_")
expect_equal(res, data.frame(id = c(1L, 1L, 2L, 2L), x = c(1L, 3L, 2L, 4L)), info = ".value with NA keeps all values")
res <- pivot_longer(df, -id, names_pattern = "(.)_.")
expect_equal(res$name, c("x", "x", "x", "x"), info = "Non-unique names from names_pattern keep all values (1)")
expect_equal(res$value, c(1L, 3L, 2L, 4L), info = "Non-unique names from names_pattern keep all values (2)")

# Columns with duplicated names are all pivoted
df <- structure(list(id = 1:2, a = 1:2, a = 3:4), names = c("id", "a", "a"), class = "data.frame", row.names = 1:2)
res <- pivot_longer(df, -id)
expect_equal(res$name, c("a", "a", "a", "a"), info = "Duplicated column names are kept")
expect_equal(res$value, c(1L, 3L, 2L, 4L), info = "Values of duplicated columns are all kept")

# names_ptypes and names_transform change the type of the name columns
df <- data.frame(id = 1, wk1 = 1, wk2 = 2)
res <- pivot_longer(df, -id, names_to = "week", names_pattern = "wk(.*)", names_transform = list(week = as.integer))
expect_equal(res$week, 1:2, info = "names_transform accepts functions")
res <- pivot_longer(df, -id, names_to = "week", names_prefix = "wk", names_transform = list(week = ~ as.integer(.x)))
expect_equal(res$week, 1:2, info = "names_transform accepts formulas")
res <- pivot_longer(df, -id, names_to = "week", names_prefix = "wk", names_ptypes = list(week = factor()))
expect_equal(res$week, factor(c("1", "2")), info = "names_ptypes casts the names")
expect_error(
  pivot_longer(df, -id, names_to = "week", names_prefix = "wk", names_ptypes = list(week = integer())),
  pattern = "Can't convert `week` <character> to <integer>"
)

# values_ptypes and values_transform change the type of the values
df <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
res <- pivot_longer(df, a:b, values_ptypes = list(value = integer()))
expect_equal(res$value, c(1L, 3L, 2L, 4L), info = "values_ptypes casts the values")
df$b <- c(3.5, 4)
expect_error(
  pivot_longer(df, a:b, values_ptypes = list(value = integer())),
  pattern = "Can't convert `b` <double> to <integer> due to loss of precision"
)
df <- data.frame(id = 1:2, a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
res <- pivot_longer(df, a:b, values_transform = list(value = as.character))
expect_equal(res$value, c("1", "x", "2", "y"), info = "values_transform makes incompatible columns compatible")

# names_repair controls clashing names
df <- data.frame(id = 1:2, a = 1:2, b = 3:4)
expect_error(pivot_longer(df, a:b, values_to = "id"), pattern = "Use `names_repair`")
res <- suppressMessages(pivot_longer(df, a:b, values_to = "id", names_repair = "unique"))
expect_equal(names(res), c("id...1", "name", "id...3"), info = "unique repair renames clashing columns")
res <- pivot_longer(df, a:b, values_to = "id", names_repair = "minimal")
expect_equal(names(res), c("id", "name", "id"), info = "minimal repair allows duplicated names")

# names_to must not contain duplicates
df <- data.frame(id = 1, a_x = 1, b_y = 2)
expect_error(pivot_longer(df, -id, names_to = c("g", "g"), names_sep = "_"), pattern = "must not be duplicated")
