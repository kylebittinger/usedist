#' Compute distances from each item to group centroids
#'
#' @param d A distance matrix object of class \code{dist}.
#' @param g A factor representing the groups of items in \code{d}.
#' @param squared If \code{TRUE}, return the squared distance to group
#'   centroids.
#' @return A data frame with distances to the group centroids:
#'
#' \describe{
#'   \item{Item}{
#'     A character vector of item labels from the dist object, or an integer
#'     vector of item locations if labels are not present.}
#'   \item{CentroidGroup}{
#'     The group for which the centroid distance is given. The column type
#'     should match that of the argument g (the \code{unique} function is used
#'     to generate this column).}
#'   \item{CentroidDistance}{
#'     Inferred distance from the item to the centroid position of the
#'     indicated group.}}
#'
#' @details
#' This function computes the distance from each item to the centroid positions
#' of groups defined in the argument \code{g}.  This is accomplished without
#' determining the centroid positions directly; see the documentation for
#' \code{\link{dist_between_centroids}} for details on this procedure.
#'
#' If the distance can't be represented in a Euclidean space, the
#' \code{CentroidDistance} is set to \code{NaN}.  See the documentation for
#' \code{\link{dist_between_centroids}} for further details.
#'
#' @export
dist_to_centroids <- function (d, g, squared = FALSE) {
  d <- stats::as.dist(d)
  d2 <- d ** 2
  items <- attr(d, "Labels")
  # Use numeric index for items if the distance matrix has no labels
  items <- if (is.null(items)) 1:attr(d, "Size") else items
  group_items <- tapply(items, g, c)
  group_sizes <- lapply(group_items, length)
  group_d2s <- lapply(group_items, function (x) dist_subset(d2, x))
  within_group_sums <- lapply(group_d2s, sum)
  df <- expand.grid(Item=items, CentroidGroup=unique(g), stringsAsFactors = F)
  dist_to_group_centroid <- function (idx2, group) {
    idx1 <- group_items[[group]]
    n1 <- group_sizes[[group]]
    sum1 <- within_group_sums[[group]]
    sum12 <- sum(as.matrix(d2)[idx1, idx2])
    term1 <- sum1 / (n1 ** 2)
    term12 <- sum12 / n1
    result_squared <- term12 - term1
    if (squared) {
      result_squared
    } else {
      is_negative <- result_squared < 0
      if (any(is_negative)) {
        msg <- paste0(
          "When computing distance to centroids, negative values were ",
          "produced before taking a square root. ",
          "This happens because the distances cannot be represented in a ",
          "Euclidean coordinate system. ",
          "These distances are being returned as NaN. ",
          "Alternately, you may set `squared = TRUE` to return the squared ",
          "distances. In this case, you will never get NaN, but you might ",
          "receive negative numbers for the squared distance.")
        warning(msg)
        result <- numeric(length(result_squared))
        result[!is_negative] <- sqrt(result_squared[!is_negative])
        result[is_negative] <- NaN
        result
      } else {
        sqrt(result_squared)
      }
    }
  }
  df$CentroidDistance <- mapply(
    dist_to_group_centroid, df$Item, df$CentroidGroup)
  df
}

#' Compute the distance between group centroids
#'
#' @param d A distance matrix object of class \code{dist}.
#' @param idx1 A vector of items in group 1.
#' @param idx2 A vector of items in group 2.
#' @param squared If \code{TRUE}, return the squared distance between centroids.
#' @return The distance between group centroids (see details).
#'
#' @details
#' If you have a distance matrix, and the objects are partitioned into groups,
#' you might like to know the distance between the group centroids. The
#' centroid of each group is simply the center of mass for the group.
#'
#' It is possible to infer the distance between group centroids directly from
#' the distances between items in each group.  The \code{adonis} test in the
#' ecology package \code{vegan} takes advantage of this approach to carry out
#' an ANOVA-like test on distances.
#'
#' The approach rests on the assumption that the objects occupy some
#' high-dimensional Euclidean space.  However, we do not have to actually
#' create the space to find the distance between centroids.  Based on the
#' assumption that such a space exists, we can use an algebraic formula to
#' perform the computation.
#'
#' The formulas for this were presented by Apostol and Mnatsakanian in 2003,
#' though we need to re-arrange equation 28 in their paper to get the value
#' we want:
#'
#' \deqn{| c_1 - c_2 | = \sqrt{
#'    \frac{1}{n_1 n_2} \sum_{(1,2)} -
#'    \frac{1}{n_1^2} \sum_{(1)} -
#'    \frac{1}{n_2^2} \sum_{(2)}},}
#'
#' where \eqn{n_1} is the number of samples in group 1, \eqn{\sum_{(1)}} is the
#' sum of squared distances between items in group 1, and \eqn{\sum_{(1,2)}} is
#' the sum of squared distances between items in group 1 and those in group 2.
#'
#' Sometimes, the distance between centroids is not a real number, because it
#' is not possible to create a space where this distance exists. Mathematically,
#' we get a negative number underneath the square root in the equation above.
#' If this happens, the function returns \code{NaN}. If you'd like to have
#' access to this value, you can set \code{squared = TRUE} to return the
#' squared distance between centroids. In this case, you will never get
#' \code{NaN}, but you might receive negative numbers in your result.
#'
#' @references Apostol, T.M. and Mnatsakanian, M.A. Sums of squares of distances
#'   in m-space. Math. Assoc. Am. Monthly 110, 516 (2003).
#'
#' @export
dist_between_centroids <- function (d, idx1, idx2, squared = FALSE) {
  if (is.logical(idx1)) {
    n1 <- sum(idx1)
  } else {
    n1 <- length(idx1)
  }
  if (is.logical(idx2)) {
    n2 <- sum(idx2)
  } else {
    n2 <- length(idx2)
  }
  d2 <- d ** 2
  sum1 <- sum(dist_subset(d2, idx1))
  sum2 <- sum(dist_subset(d2, idx2))
  sum12 <- sum(as.matrix(d2)[idx1, idx2])
  term1 <- sum1 / (n1 ** 2)
  term2 <- sum2 / (n2 ** 2)
  term12 <- sum12 / (n1 * n2)
  result_squared <- term12 - term1 - term2
  if (squared) {
    result_squared
  } else {
    is_negative <- result_squared < 0
    if (any(is_negative)) {
      msg <- paste0(
        "When computing distance between centroids, negative values were ",
        "produced before taking a square root. ",
        "This happens because the distances cannot be represented in a ",
        "Euclidean coordinate system. ",
        "These distances are being returned as NaN. ",
        "Alternately, you may set `squared = TRUE` to return the squared ",
        "distances. In this case, you will never get NaN, but you might ",
        "receive negative numbers for the squared distance.")
      warning(msg)
      result <- numeric(length(result_squared))
      result[!is_negative] <- sqrt(result_squared[!is_negative])
      result[is_negative] <- NaN
      result
    } else {
      sqrt(result_squared)
    }
  }
}

#' Make a new distance matrix of centroid distances between multiple groups
#' @param d A distance matrix object of class \code{dist}.
#' @param g A factor representing the groups of items in \code{d}.
#' @param squared If \code{TRUE}, return the squared distance between centroids.
#' @return A distance matrix of distances between the group centroids.
#' @export
dist_multi_centroids <- function (d, g, squared = FALSE) {
  group_idxs <- tapply(seq_along(g), g, c, simplify = FALSE)
  centroid_distance_from_groups <- function (gg) {
    g1 <- gg[1]
    g2 <- gg[2]
    idx1 <- group_idxs[[g1]]
    idx2 <- group_idxs[[g2]]
    dist_between_centroids(d, idx1, idx2, squared = squared)
  }
  dc <- utils::combn(names(group_idxs), 2, centroid_distance_from_groups)
  attr(dc, "Size") <- length(names(group_idxs))
  attr(dc, "Labels") <- names(group_idxs)
  attr(dc, "Diag") <- FALSE
  attr(dc, "Upper") <- FALSE
  class(dc) <- "dist"
  dc
}
