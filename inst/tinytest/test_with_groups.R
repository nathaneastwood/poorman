df <- data.frame(x = 1:2)
gf <- group_by(df, x)
expect_equal(class(with_groups(df, x, mutate)), "data.frame", info = "restores original class #1")
expect_equal(class(with_groups(gf, x, mutate)), "grouped_df", info = "restores original class #2")

gf <- group_by(data.frame(x = 1:2), x)
out <- gf %>% with_groups(NULL, mutate, y = mean(x))
expect_equal(out$y, c(1.5, 1.5), info = ".groups = NULL ungroups")

local_fn <- identity
expect_identical(
  with_groups(mtcars, local_fn(2), mutate, disp = disp / sd(disp)),
  with_groups(mtcars, 2, mutate, disp = disp / sd(disp)),
  info = ".groups is defused with context"
)
