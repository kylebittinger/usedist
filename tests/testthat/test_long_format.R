context("pivot_to_numeric_matrix")

data_long <- data.frame(
  Observation = rep(c("A", "B", "C"), each = 5),
  Feature = paste0("F", c(1:5, 2:6, 4:8)),
  Val = 1:15,
  stringsAsFactors = FALSE)

data_wide <- matrix(
  c(
    1, 2, 3,  4,  5,  0,  0,  0,
    0, 6, 7,  8,  9, 10,  0,  0,
    0, 0, 0, 11, 12, 13, 14, 15),
  byrow = TRUE, nrow=3,
  dimnames = list(c("A", "B", "C"), paste0("F", 1:8)))

test_that("Data frame in long format converted to numeric matrix", {
  expect_equal(
    pivot_to_numeric_matrix(data_long, Observation, Feature, Val),
    data_wide)
})

test_that("Extra columns are discarded", {
  data_long$Extra1 <- rnorm(15)
  data_long$Extra2 <- LETTERS[1:15]
  expect_equal(ncol(data_long), 5)
  expect_equal(
    pivot_to_numeric_matrix(data_long, Observation, Feature, Val),
    data_wide)
})
