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

# dist_between_centroids

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

# dist_to_centroids

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

# dist_multi_centroids

test_that("dist_multi_centroids works with multiple groups", {
  g2 <- paste("Group", rep(1:4, each=2))
  g2_centroids <- matrix(c(0, 0, 1, -1, 3, 0, 4, -1), nrow = 4, byrow = TRUE)
  g2_centroid_dist <- dist(g2_centroids)
  expect_equal(
    as.numeric(dist_multi_centroids(pts_d, g2)),
    as.numeric(g2_centroid_dist))
})
