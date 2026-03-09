context("dist_read/write")

test_that("read_distance_matrix works", {
  expected <- matrix(
    c(
      0.0000, 0.6569, 0.6562, 0.5400,
      0.6569, 0.0000, 0.6729, 0.6317,
      0.6562, 0.6729, 0.0000, 0.5898,
      0.5400, 0.6317, 0.5898, 0.0000
      ),
    ncol = 4,
    dimnames = list(
      c("sA", "sB", "sC", "sD"),
      c("sA", "sB", "sC", "sD")
      )
    )
  expected <- as.dist(expected)
  fp <- system.file("extdata", "distance-matrix.tsv", package = "usedist")
  observed <- dist_read(fp)

  local_edition(3)
  expect_equal(observed, expected, ignore_attr = "call")
})