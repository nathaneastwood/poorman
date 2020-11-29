# has_rownames()
expect_false(has_rownames(iris), info = "has_rownames() returns FALSE correctly")
expect_true(has_rownames(mtcars), info = "has_rownames() returns TRUE correctly")
expect_false(has_rownames(1:10), info = "has_rownames() works on other objects, returning FALSE")

# remove_rownames()
expect_false(has_rownames(remove_rownames(mtcars)), info = "remove_rownames() acts accordingly")
expect_false(
  has_rownames(remove_rownames(iris)),
  info = "remove_rownames() acts accordingly for datasets without rownames"
)

# rowid_to_column()
res <- rowid_to_column(mtcars)
expect_false(has_rownames(res), info = "rownames are correctly removed")
expect_equal(
  mtcars %>% group_by(am) %>% rowid_to_column() %>% class(),
  c("grouped_data", "data.frame"),
  info = "rowid_to_column() retains classes"
)
expect_equal(res$rowid, seq_len(nrow(mtcars)), info = "rowid is correctly set")

# rownames_to_column()
res <- rownames_to_column(mtcars)
expect_false(has_rownames(res), info = "rownames have indeed been removed")
expect_equal(res$rowname, rownames(mtcars), info = "rownames have been correctly converted to a column")
expect_equal(
  mtcars %>% group_by(am) %>% rownames_to_column() %>% class(),
  c("grouped_data", "data.frame"),
  info = "rownames_to_column() retains class"
)

# column_to_rownames()
res <- column_to_rownames(res, "rowname")
expect_true(has_rownames(res), info = "column_to_rownames() does set rownames")
expect_equal(rownames(res), rownames(mtcars), info = "rownames are correctly set")
expect_equal(res, mtcars)
expect_false("rowname" %in% colnames(res), info = "The old column is removed")
expect_equal(
  mtcars %>% group_by(am) %>% rownames_to_column() %>% column_to_rownames("rowname") %>% class(),
  c("grouped_data", "data.frame"),
  info = "column_to_rownames() retains class"
)
