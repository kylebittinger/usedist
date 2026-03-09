d <- matrix(
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
d <- as.dist(d)

d_txt <- c(
  "\tsA	sB	sC	sD",
  "sA	0	0.6569	0.6562	0.54",
  "sB	0.6569	0	0.6729	0.6317",
  "sC	0.6562	0.6729	0	0.5898",
  "sD	0.54	0.6317	0.5898	0"
)

# dist_read

test_that("dist_read works", {
  fp <- system.file("extdata", "distance-matrix.tsv", package = "usedist")
  observed <- dist_read(fp)

  local_edition(3)
  expect_equal(observed, d, ignore_attr = "call")
})

# dist_write

test_that("dist_write works", {
  fp <- tempfile(fileext = ".tsv")
  dist_write(d, fp)
  expect_equal(readLines(fp), d_txt)
  file.remove(fp)
})

test_that("dist_write and dist_read preserve distance matrix", {
  fp <- tempfile(fileext = ".tsv")
  dist_write(d, fp)
  d_observed <- dist_read(fp)
  file.remove(fp)

  local_edition(3)
  expect_equal(d_observed, d, ignore_attr = "call")
})
