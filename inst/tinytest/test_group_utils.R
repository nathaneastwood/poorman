combine_data_frames <- poorman:::combine_data_frames

expect_equal(
  combine_data_frames(list(data.frame(x = 1:2, y = c("a", "b")), data.frame(x = 3L, y = "c"))),
  data.frame(x = 1:3, y = c("a", "b", "c")),
  info = "combine_data_frames() binds data frames by row"
)
expect_equal(
  combine_data_frames(list(data.frame(x = 1L), data.frame(x = 2.5), data.frame(x = TRUE))),
  data.frame(x = c(1, 2.5, 1)),
  info = "combine_data_frames() combines logical, integer and double columns into the richest type"
)
expect_error(
  combine_data_frames(list(data.frame(x = as.raw(1)), data.frame(x = 2L))),
  info = "combine_data_frames() does not combine raw and integer columns, as rbind() does not"
)
expect_equal(
  combine_data_frames(list(data.frame(x = TRUE), data.frame(x = "a"))),
  data.frame(x = c("TRUE", "a")),
  info = "combine_data_frames() combines columns of other types as rbind() does"
)

rm(combine_data_frames)
