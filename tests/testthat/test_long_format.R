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

context("dist_long")

data_manhattan_dist <- structure(
    c(27, 62, 51), Size = 3L, Labels = c("A", "B", "C"),
    Diag = FALSE, Upper = FALSE, class = "dist")

manhattan <- function (x, y) {
  sum(abs(y - x))
}

test_that("Manhattan distance works for data in long format", {
  expect_equal(
    dist_long(data_long, Observation, Feature, Val, manhattan),
    data_manhattan_dist)
})
