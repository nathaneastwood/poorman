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
