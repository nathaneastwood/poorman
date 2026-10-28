df <- data.frame(key = c("x", "y", "z"), val = 1:3)
pv <- pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(names(pv), c("x", "y", "z"))
expect_equal(nrow(pv), 1)

df <- data.frame(a = 1, key = c("x", "y"), val = 1:2)
pv <- pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(names(pv), c("a", "x", "y"))
expect_equal(nrow(pv), 1)

df <- data.frame(a = 1:2, key = c("x", "y"), val = 1:2)
pv <- pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(pv$a, c(1, 2))
expect_equal(pv$x, c(1, NA))
expect_equal(pv$y, c(NA, 2))

df <- data.frame(
  a = c(1, 1),
  key = c("a", "b"),
  val = c(1, 2)
)

expect_error(
  pivot_wider(df, names_from = "key", values_from = "val"),
  pattern = "Some values of the columns specified"
)

df <- data.frame(
  variable = c("a", "b", "c"),
  date = c("2015-01-01", "2015-01-01", "2015-01-02"),
  value = c(1, 2, 3),
  stringsAsFactors = FALSE
)

res <- df %>% pivot_wider(
  names_from = "date",
  values_from = "value",
  values_fill = 0
)

expect_equal(
  res,
  structure(
    list(variable = c("a", "b", "c"), `2015-01-01` = c(1, 2, 0), `2015-01-02` = c(0, 0, 3)),
    row.names = c(NA, 3L),
    class = "data.frame"
  )
)


production <- expand.grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
) %>%
  filter((product == "A" & country == "AI") | product == "B")

production$production <- rnorm(nrow(production))

res <- production %>%
  pivot_wider(
    names_from = c("product", "country"),
    values_from = "production"
  )
expect_equal(dim(res), c(15, 4))
expect_equal(colnames(res), c("year", "A_AI", "B_AI", "B_EI"))

res <- production %>%
  pivot_wider(
    names_from = c("product", "country"),
    values_from = "production",
    names_glue = "prod_{product}_{country}"
  )
expect_equal(colnames(res), c("year", "prod_A_AI", "prod_B_AI", "prod_B_EI"))

res <- production %>%
  pivot_wider(
    names_from = c("product", "country"),
    values_from = "production",
    names_sep = "."
  )

expect_equal(colnames(res), c("year", "A.AI", "B.AI", "B.EI"))

contacts <- data.frame(
  field = c("name", "company", "name", "company", "email", "name"),
  value = c("Jiena McLellan", "Toyota", "John Smith", "google", "john@google.com", "Huxley Ratcliffe"),
  stringsAsFactors = FALSE
)
contacts$person_id <- cumsum(contacts$field == "name")

res <- contacts %>%
  pivot_wider(names_from = "field", values_from = "value")

expect_equal(
  res,
  structure(
    list(
      person_id = 1:3,
      name = c("Jiena McLellan", "John Smith", "Huxley Ratcliffe"),
      company = c("Toyota", "google", NA),
      email = c(NA, "john@google.com", NA)
    ),
    row.names = c(NA, 3L),
    class = "data.frame"
  )
)

df <- data.frame(
  food = c("banana", "banana", "banana", "banana", "cheese", "cheese", "cheese", "cheese"),
  binary = c(rep(c("yes", "no"), 4)),
  car = c("toyota", "subaru", "mazda", "skoda", "toyota", "subaru", "mazda", "skoda"),
  fun = c(2, 4, 3, 6, 2, 4, 2, 3),
  stringsAsFactors = FALSE
)

res <- df %>%
  pivot_wider(
    id_cols = "food",
    names_from = c("car", "binary"),
    names_glue = "{binary}_{car}",
    values_from = "fun"
  )

expect_equal(
  res,
  structure(
    list(
      food = c("banana", "cheese"), yes_toyota = c(2, 2), no_subaru = c(4, 4), yes_mazda = c(3, 2), no_skoda = c(6, 3)
    ),
    row.names = 1:2,
    class = "data.frame"
  )
)

# id_cols selects the id columns, dropping any others, in the order given
df <- data.frame(id = c(1, 1, 2), g = c("x", "y", "z"), nm = c("a", "b", "a"), v = 1:3, stringsAsFactors = FALSE)
res <- pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "v")
expect_equal(res, data.frame(id = c(1, 2), a = c(1L, 3L), b = c(2L, NA)), info = "Columns not in id_cols are dropped")
res <- pivot_wider(df[c(1, 3), ], id_cols = c("g", "id"), names_from = "nm", values_from = "v")
expect_equal(names(res), c("g", "id", "a"), info = "id_cols are returned in the order given")
expect_equal(
  pivot_wider(df, id_cols = id, names_from = nm, values_from = v),
  pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "v"),
  info = "Bare column names can be used for id_cols"
)

# Character vectors held in variables can be used to select columns
nf <- "nm"
vf <- "v"
expect_equal(
  pivot_wider(df, id_cols = "id", names_from = nf, values_from = vf),
  pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "v"),
  info = "Variables can be used for names_from and values_from"
)

# The class and attributes of the values are kept
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), stringsAsFactors = FALSE)
df$d <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
df$f <- factor(c("x", "y", "x"), levels = c("y", "x"))
res <- pivot_wider(df, names_from = "nm", values_from = "d", id_cols = "id")
expect_equal(res$a, as.Date(c("2020-01-01", "2020-01-03")), info = "Dates are kept")
expect_equal(res$b, as.Date(c("2020-01-02", NA)), info = "Missing dates are NA")
res <- pivot_wider(df, names_from = "nm", values_from = "f", id_cols = "id")
expect_equal(res$a, factor(c("x", "x"), levels = c("y", "x")), info = "Factors and their levels are kept")

# values_fill only fills missing combinations and keeps the type of the values
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), v = c(NA, 2L, 3L), stringsAsFactors = FALSE)
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fill = 0)
expect_equal(res$a, c(NA, 3L), info = "Explicit missing values are not filled")
expect_equal(res$b, c(2L, 0L), info = "Integer columns are filled and stay integer")
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fill = list(v = 0L))
expect_equal(res$b, c(2L, 0L), info = "values_fill can be a named list")
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fill = 0, names_prefix = "p_")
expect_equal(res$p_b, c(2L, 0L), info = "values_fill works with names_prefix")
df$l <- c(TRUE, FALSE, TRUE)
res <- pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "l", values_fill = FALSE)
expect_equal(res$b, c(FALSE, FALSE), info = "Logical columns are filled")
res <- pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "l", values_fill = 1)
expect_equal(res$b, c(FALSE, TRUE), info = "0 and 1 can fill logical columns")
expect_error(
  pivot_wider(df, id_cols = "id", names_from = "nm", values_from = "l", values_fill = "a"),
  pattern = "`values_fill` must be of type logical"
)

# Several values_from columns are named <value><sep><prefix><name>
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), v = 1:3, w = 4:6, stringsAsFactors = FALSE)
res <- pivot_wider(df, names_from = "nm", values_from = c("v", "w"), names_prefix = "P")
expect_equal(names(res), c("id", "v_Pa", "v_Pb", "w_Pa", "w_Pb"), info = "names_prefix goes after the value name")

# names_glue works with a single names_from column
res <- pivot_wider(df, names_from = "nm", values_from = "v", id_cols = "id", names_glue = "x_{nm}")
expect_equal(names(res), c("id", "x_a", "x_b"), info = "names_glue works with one names_from column")
res <- pivot_wider(df, names_from = "nm", values_from = c("v", "w"), names_glue = "{nm}.{.value}")
expect_equal(names(res), c("id", "a.v", "b.v", "a.w", "b.w"), info = "names_glue can use .value")

# Values which are not uniquely identified create list-columns
df <- data.frame(id = c(1, 1, 2), nm = c("a", "a", "a"), v = 1:3, stringsAsFactors = FALSE)
expect_warning(
  res <- pivot_wider(df, names_from = "nm", values_from = "v"),
  pattern = "Values are not uniquely identified; output will contain list-cols"
)
expect_equal(res$a, list(1:2, 3L), info = "Duplicate values are kept in a list-column")

# The class of the input is kept, e.g. for tibbles
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), v = 1:3, stringsAsFactors = FALSE)
tbl <- structure(df, class = c("tbl_df", "tbl", "data.frame"))
res <- pivot_wider(tbl, names_from = "nm", values_from = "v")
expect_equal(class(res), c("tbl_df", "tbl", "data.frame"), info = "The input class is kept")
expect_equal(res$b, c(2L, NA), info = "Values of tibble-like input are correct")

# Zero row data can be pivoted
res <- pivot_wider(df[0, ], names_from = "nm", values_from = "v")
expect_equal(res, data.frame(id = numeric()), info = "Zero row data returns a zero row data.frame")

# Column order follows the order of appearance of the names, including for factors
df <- data.frame(id = 1, nm = factor(c("b", "a"), levels = c("a", "b")), v = 1:2)
expect_equal(names(pivot_wider(df, names_from = "nm", values_from = "v")), c("id", "b", "a"))

# names_sort sorts the new columns: factors by level, characters in C locale order with NA last
df <- data.frame(id = 1, nm = c("b", "B", NA, "a"), v = 1:4, stringsAsFactors = FALSE)
expect_equal(
  names(pivot_wider(df, names_from = "nm", values_from = "v", names_sort = TRUE)),
  c("id", "B", "a", "b", "NA"),
  info = "names_sort sorts character names"
)
res <- pivot_wider(df, names_from = "nm", values_from = "v", names_sort = TRUE)
expect_equal(unname(unlist(res[-1])), c(2L, 4L, 1L, 3L), info = "Values follow their sorted columns")
df <- data.frame(id = 1, nm = factor(c("b", "a"), levels = c("b", "a")), v = 1:2)
expect_equal(
  names(pivot_wider(df, names_from = "nm", values_from = "v", names_sort = TRUE)), c("id", "b", "a"),
  info = "names_sort sorts factors by their levels"
)

# values_fn summarises the values of each cell
df <- data.frame(id = c(1, 1, 2), nm = c("a", "a", "a"), v = 1:3, w = 4:6, stringsAsFactors = FALSE)
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fn = sum, id_cols = "id")
expect_equal(res$a, c(3L, 3L), info = "values_fn can be a function")
expect_warning(
  res <- pivot_wider(df, names_from = "nm", values_from = c("v", "w"), values_fn = list(v = length)),
  pattern = "not uniquely identified"
)
expect_equal(res$v_a, c(2L, 1L), info = "values_fn can be a named list")
expect_equal(res$w_a, list(4:5, 6L), info = "Values without a values_fn give list-columns")
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fn = list(v = ~ max(.x)), id_cols = "id")
expect_equal(res$a, c(2L, 3L), info = "values_fn can use formulas in a named list")
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fn = list, id_cols = "id")
expect_equal(res$a, list(1:2, 3L), info = "values_fn = list gives a list-column without a warning")
df <- data.frame(id = c(1, 2), nm = c("a", "b"), v = 1:2, stringsAsFactors = FALSE)
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fn = function(x) x * 10)
expect_equal(res$a, c(10, NA), info = "values_fn is applied when values are unique")
res <- pivot_wider(df, names_from = "nm", values_from = "v", values_fn = length, values_fill = 0)
expect_equal(res$b, c(0L, 1L), info = "values_fill is cast to the type of the summarised values")
expect_error(
  pivot_wider(df, names_from = "nm", values_from = "v", values_fn = ~ sum(.x)),
  pattern = "`values_fn` must be a NULL, a function, or a named list"
)

# names_repair controls clashing and non-syntactic names
df <- data.frame(id = c(1, 2), nm = c("id", "1"), v = 1:2, stringsAsFactors = FALSE)
expect_error(pivot_wider(df, names_from = "nm", values_from = "v"), pattern = "Use `names_repair`")
expect_message(
  res <- pivot_wider(df, names_from = "nm", values_from = "v", names_repair = "unique"),
  pattern = "New names"
)
expect_equal(names(res), c("id...1", "id...2", "1"), info = "unique repair renames all duplicates")
res <- suppressMessages(pivot_wider(df, names_from = "nm", values_from = "v", names_repair = "universal"))
expect_equal(names(res), c("id...1", "id...2", "...1"), info = "universal repair makes syntactic names")
res <- pivot_wider(df, names_from = "nm", values_from = "v", names_repair = "minimal")
expect_equal(names(res), c("id", "id", "1"), info = "minimal repair keeps the names")
res <- pivot_wider(df, names_from = "nm", values_from = "v", names_repair = toupper)
expect_equal(names(res), c("ID", "ID", "1"), info = "names_repair can be a function")

# The new column names must be unique whatever the names_repair strategy
df <- data.frame(id = 1, nm = c("a", "b"), v = 1:2, stringsAsFactors = FALSE)
expect_error(
  pivot_wider(df, names_from = "nm", values_from = "v", names_glue = "same", names_repair = "unique"),
  pattern = "already present"
)

# values_fill must not lose precision and can be NA
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), v = c(1L, 2L, 3L), stringsAsFactors = FALSE)
expect_error(
  pivot_wider(df, names_from = "nm", values_from = "v", values_fill = 0.5),
  pattern = "due to loss of precision"
)
expect_equal(pivot_wider(df, names_from = "nm", values_from = "v", values_fill = NA)$b, c(2L, NA))

expect_equal(
  names(formals(pivot_wider)),
  c("data", "id_cols", "names_from", "names_prefix", "names_sep", "names_glue", "names_sort", "names_repair",
    "values_from", "values_fill", "values_fn", "..."),
  info = "pivot_wider() arguments are in the documented order"
)
df <- data.frame(id = c(1, 1, 2), nm = c("a", "b", "a"), v = 1:3, stringsAsFactors = FALSE)
expect_equal(
  pivot_wider(df, "id", "nm", "p_", values_from = "v"),
  pivot_wider(df, id_cols = "id", names_from = "nm", names_prefix = "p_", values_from = "v"),
  info = "Positional arguments are in the documented order"
)
