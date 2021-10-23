context("pivot_to_numeric_matrix")

data_long <- data.frame(
  row_id = rep(c("A", "B", "C"), each = 5),
  col_id = paste0("F", c(1:5, 2:6, 4:8)),
  val_num = 1:15,
  val_char = letters[1:15],
  stringsAsFactors = FALSE)

data_wide <- matrix(
  c(
    1, 2, 3,  4,  5,  0,  0,  0,
    0, 6, 7,  8,  9, 10,  0,  0,
    0, 0, 0, 11, 12, 13, 14, 15),
  byrow = TRUE, nrow=3,
  dimnames = list(c("A", "B", "C"), paste0("F", 1:8)))

data_wide_char <- matrix(
  c(
    "a", "b", "c", "d", "e",  "",  "",  "",
     "", "f", "g", "h", "i", "j",  "",  "",
     "",  "",  "", "k", "l", "m", "n", "o"),
  byrow = TRUE, nrow=3,
  dimnames = list(c("A", "B", "C"), paste0("F", 1:8)))

test_that("Data frame in long format converted to numeric matrix", {
  expect_equal(
    pivot_to_numeric_matrix(data_long, row_id, col_id, val_num),
    data_wide)
})

test_that("pivot_to_matrix works with character values", {
  expect_equal(
    pivot_to_matrix(data_long, row_id, col_id, val_char, fill = ""),
    data_wide_char)
})

test_that("pivot_to_matrix works with numeric values", {
  expect_equal(
    pivot_to_matrix(data_long, row_id, col_id, val_num),
    data_wide)
})
