dm_names <- letters[1:4]
dm <- matrix(
  c(0, 1, 2, 3,
    1, 0, 4, 5,
    2, 4, 0, 6,
    3, 5, 6, 0),
  ncol=4,
  dimnames=list(dm_names, dm_names))
dm <- as.dist(dm)

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

test_that('dist_subset returns a dist object', {
  expect_equal(class(dist_subset(dm, 1:3)), "dist")
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
  # We don't set the call attribute, so remove this from the expected result
  attr(expected_dm, "call") <- NULL

  manhattan_distance <- function (v1, v2) sum(abs(v1 - v2))
  observed_dm <- dist_make(x, manhattan_distance, "manhattan")
  expect_equal(observed_dm, expected_dm)
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

context("dist_between_centroids")

test_that("dist_between_centroids works on 2D Euclidean example", {
  expect_equal(dist_between_centroids(pts_d, 1:4, 5:8), 3.0)
})

context("dist_to_centroids")

test_that("dist_to_centroids works on 2D Euclidean example", {
  expected_df <- expand.grid(
    Item = LETTERS[1:8],
    CentroidGroup = paste("Group", 1:2),
    stringsAsFactors=FALSE)
  expected_df$CentroidDistance = c(
      1, 1, 1, 1, 2, sqrt(10), sqrt(10), 4,
      4, sqrt(10), sqrt(10), 2, 1, 1, 1, 1)
  expect_equal(dist_to_centroids(pts_d, pts_g), expected_df)
})
