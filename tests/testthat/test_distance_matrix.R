dm_names <- letters[1:4]
dm <- matrix(
  c(0, 1, 2, 3,
    1, 0, 4, 5,
    2, 4, 0, 6,
    3, 5, 6, 0),
  ncol=4,
  dimnames=list(dm_names, dm_names))
dm <- as.dist(dm)

dm_124 <- matrix(
  c(0, 1, 3,
    1, 0, 5,
    3, 5, 0),
  ncol = 3,
  dimnames = list(c("a", "b", "d"), c("a", "b", "d")))
dm_124 <- as.dist(dm_124)
# The call attribute is different depending on how the distance matrix
# was generated. Remove it for the tests.
attr(dm_124, "call") <- NULL

context('dist_setNames')

test_that('dist_setNames can set the labels', {
  EFGH <- LETTERS[5:8]
  renamed_dm <- dist_setNames(dm, EFGH)
  expect_equal(attr(renamed_dm, "Labels"), EFGH)
  expect_equal(class(renamed_dm), "dist")
})

context('dist_get')

test_that('dist_get works with named indices', {
  expect_equal(
    dist_get(dm, c("a", "b"), c("c", "d")), c(2, 5))
  expect_equal(
    dist_get(dm, c("a", "a", "a"), c("a", "b", "c")), c(0, 1, 2))
})

test_that('dist_get recycles short vectors', {
  expect_equal(dist_get(dm, "a", c("a", "b", "c")), c(0, 1, 2))
})

test_that('dist_get works with numeric indices', {
  expect_equal(dist_get(dm, c(1, 3), c(4, 4)), c(3, 6))
})


context('dist_subset')

test_that('dist_subset works with numeric vectors', {
  res <- dist_subset(dm, c(1, 2, 4))
  attr(res, "call") <- NULL
  expect_equal(res, dm_124)
})

test_that('dist_subset works with boolean vectors', {
  res <- dist_subset(dm, c(TRUE, TRUE, FALSE, TRUE))
  attr(res, "call") <- NULL
  expect_equal(res, dm_124)
})

test_that('dist_subset works with named vectors', {
  res <- dist_subset(dm, c("a", "b", "d"))
  attr(res, "call") <- NULL
  expect_equal(res, dm_124)
})

context("dist_groups")

test_that("dist_groups labels groups correctly", {
  dg <- dist_groups(dm, c("A", "A", "B", "B"))
  expect_equal(levels(dg$Label), c("Between A and B", "Within A", "Within B"))
  dg <- dist_groups(dm, c("A", "B", "A", "B"))
  expect_equal(levels(dg$Label), c("Between A and B", "Within A", "Within B"))
})

context("dist_make")

test_that("dist_make computes custom distances", {
  x <- matrix(sin(1:30), nrow=5)
  expected_dm <- dist(x, method="manhattan")
  # We don't set the call or method attributes; remove from the expected result
  attr(expected_dm, "call") <- NULL
  attr(expected_dm, "method") <- NULL

  manhattan_distance <- function (v1, v2) sum(abs(v1 - v2))
  observed_dm <- dist_make(x, manhattan_distance)
  expect_equal(observed_dm, expected_dm)
})

test_that("dist_make passes additional arguments to distance function", {
  constant_dist <- function (v1, v2, constant = 0) constant
  m <- rbind(a = c(1, 2, 3), b = c(1, 8, 5), c = c(0, 0, 0))
  expect_equal(as.numeric(dist_make(m, constant_dist)), rep(0, 3))
  expect_equal(as.numeric(dist_make(m, constant_dist, constant = 4)), rep(4, 3))
  expect_equal(as.numeric(dist_make(m, constant_dist, 6)), rep(6, 3))
})

pts <- matrix(c(
  -1,  0,
   0,  1,
   0, -1,
   1,  0,
   2,  0,
   3,  1,
   3, -1,
   4,  0), ncol = 2, byrow = TRUE)
rownames(pts) <- LETTERS[1:8]
pts_d <- dist(pts)
pts_g <- rep(paste("Group", 1:2), each=4)

# Similar to the example in Figure 9.17 of Numerical Ecology
noneuclidean <- matrix(
  c(0, 0.8, 0.8, 0.3,
    0.8, 0, 0.8, 0.3,
    0.8, 0.8, 0, 0.3,
    0.3, 0.3, 0.3, 0),
  ncol=4, byrow=T)
noneuclidean <- as.dist(noneuclidean)

context("dist_between_centroids")

test_that("dist_between_centroids works on 2D Euclidean example", {
  expect_equal(dist_between_centroids(pts_d, 1:4, 5:8), 3.0)
})

test_that("dist_between_centroids returns squared distances", {
  expect_equal(dist_between_centroids(pts_d, 1:4, 5:8, squared = TRUE), 9.0)
})

test_that("dist_between_centroids works with logical indices", {
  group1 <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(dist_between_centroids(pts_d, group1, !group1), 3.0)
})

test_that("dist_between_centroids returns NaN if distance is non-Euclidean", {
  expect_warning(
    res <- dist_between_centroids(noneuclidean, 1:3, 4),
    "negative values")
  expect_identical(res, NaN)
  # Distance between centroids for samples 1, 2, and 3 is Euclidean
  expect_equal(dist_between_centroids(noneuclidean, 1:2, 3), sqrt(3 / 4) * 0.8)
})

context("dist_to_centroids")

expected_centroid_df <- expand.grid(
  Item = LETTERS[1:8],
  CentroidGroup = paste("Group", 1:2),
  stringsAsFactors=FALSE)
expected_centroid_df$CentroidDistance = c(
  1, 1, 1, 1, 2, sqrt(10), sqrt(10), 4,
  4, sqrt(10), sqrt(10), 2, 1, 1, 1, 1)

test_that("dist_to_centroids works on 2D Euclidean example", {
  expect_equal(dist_to_centroids(pts_d, pts_g), expected_centroid_df)
})

test_that("dist_to_centroids returns squared distances", {
  sq_df <- expected_centroid_df
  sq_df$CentroidDistance <- sq_df$CentroidDistance ** 2
  expect_equal(dist_to_centroids(pts_d, pts_g, squared=TRUE), sq_df)
})

test_that("dist_to_centroids returns NaN if the distance is non-Euclidean", {
  expect_warning(
    res <- dist_to_centroids(noneuclidean, c(1,2,2,2)),
    "negative values")
  expect_identical(res$CentroidDistance[8], NaN)
  expect_equal(res$CentroidDistance[2], 0.8)
})

context("dist_multi_centroids")

test_that("dist_multi_centroids works with multiple groups", {
  g2 <- paste("Group", rep(1:4, each=2))
  g2_centroids <- matrix(c(0, 0, 1, -1, 3, 0, 4, -1), nrow = 4, byrow = TRUE)
  g2_centroid_dist <- dist(g2_centroids)
  expect_equal(
    as.numeric(dist_multi_centroids(pts_d, g2)),
    as.numeric(g2_centroid_dist))
})
