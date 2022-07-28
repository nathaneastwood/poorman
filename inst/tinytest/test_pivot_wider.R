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
