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

test_that("pivot_to_numeric_matrix works", {
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

test_that("error_message_for_pkg_function works for missing package", {
  expect_equal(
    error_message_for_pkg_function(c("notinstalled", "notafunction")),
    "Package notinstalled is not installed.")
})

test_that("error_message_for_pkg_function works for missing function", {
  expect_equal(
    error_message_for_pkg_function(c("stats", "notafunction")),
    "Package stats is installed but function notafunction is not available.")
})

test_that("error_message_for_pkg_function works for available functions", {
  expect_equal(
    error_message_for_pkg_function(c("stats", "lm")),
    NA_character_)
})

test_that("check_pkg_functions stops on missing function", {
  expect_error(
    check_pkg_functions(c("stats", "notafunction")),
    "The following packages or functions are not available: ")
})
