expect_equal(
  mtcars %>% select(MilesPerGallon = mpg, NumberOfGears = gear),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = 1, NumberOfGears = 10),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using integers"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = "mpg", NumberOfGears = "gear"),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using characters"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = 1, Horsepower = "hp", NumberOfGears = gear),
  {
    res <- mtcars[, c("mpg", "hp", "gear")]
    colnames(res) <- c("MilesPerGallon", "Horsepower", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using a mixture of integer, character and nse names"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = -mpg) %>% colnames(),
  colnames(mtcars)[-1L],
  info = "Negative column selections do not affect renaming"
)

expect_equal(
  mtcars %>% select(Columns = -mpg:-gear) %>% colnames(),
  "carb",
  info = "Negative column selections do not affect renaming in multiple column selections"
)

expect_equal(
  mtcars %>% select(Col = mpg:disp) %>% colnames(),
  c("Col1", "Col2", "Col3"),
  info = "Renaming columns during multiple column selection"
)

expect_equal(
  mtcars %>% select(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
  mtcars[, rev(colnames(mtcars))],
  info = "Test order of selection is preserved"
)

expect_equal(
  mtcars %>% select(1, 1, 1, 1),
  mtcars[, "mpg", drop = FALSE],
  info = "Test selecting the same column multiple times only returns it once"
)

expect_equal(
  mtcars %>% select(mpg:disp, Gears = gear),
  {
    res <- mtcars[, c("mpg", "cyl", "disp", "gear")]
    colnames(res)[4] <- "Gears"
    res
  },
  info = "Ensure columns are named correctly when not all columns are to be renamed"
)

res <- suppressMessages(mtcars %>% group_by(cyl) %>% select(MilesPerGallon = mpg))
expect_equal(
  colnames(res),
  c("cyl", "MilesPerGallon"),
  info = "Ensure columns are named correctly when renaming grouped data"
)

df <- data.frame(id = 1:2, g = c("a", "b"), v = 3:4, w = 5:6)
gdf <- group_by(df, id)

expect_equal(
  select(gdf, -g),
  group_by(df[, c("id", "v", "w")], id),
  info = "A negative selection works on grouped data"
)

expect_equal(
  colnames(select(gdf, -c(g, v))),
  c("id", "w"),
  info = "A negative selection of several columns works on grouped data"
)

expect_equal(
  colnames(select(gdf, !(g:v))),
  c("id", "w"),
  info = "A negated range works on grouped data"
)

expect_equal(
  colnames(select(gdf, -starts_with("g"))),
  c("id", "v", "w"),
  info = "A negated selection helper works on grouped data"
)

expect_equal(
  colnames(select(group_by(df, v), -id)),
  c("g", "v", "w"),
  info = "Grouping variables kept by a negative selection stay in place"
)

expect_message(
  res <- select(gdf, -id),
  "Adding missing grouping variables: `id`",
  info = "Deselecting a grouping variable adds it back with a message"
)
expect_equal(colnames(res), c("id", "g", "v", "w"), info = "A deselected grouping variable is added back first")

res <- suppressMessages(select(group_by(df, w, id), -c(id, w)))
expect_equal(
  list(colnames(res), group_vars(res)),
  list(c("w", "id", "g", "v"), c("w", "id")),
  info = "Deselected grouping variables are added back in the order of the groups"
)

res <- suppressMessages(select(gdf, -everything()))
expect_equal(colnames(res), "id", info = "Deselecting everything on grouped data keeps the grouping variables")

expect_message(
  res <- select(group_by(df, w, id), starts_with("i")),
  "Adding missing grouping variables: `w`\n",
  info = "The message only names the grouping variables which are added back"
)
expect_equal(colnames(res), c("w", "id"), info = "Selected grouping variables are not added twice")

ce_nm <- "tot"
expect_equal(
  names(select(data.frame(x = 1, y = 2), "{ce_nm}" := y)), # nolint: object_name_linter.
  "tot",
  info = "select() renames with :="
)
