#' usedist: a package for working with distance matrices in R
#'
#' In usedist, we provide a number of functions to help with distance matrix
#' objects, such as those produced by the `dist` function.  Some functions are
#' geared towards making or altering distance matrix objects.  Others relate to
#' groups of items in the distance matrix. They provide access to within- or
#' between-group distances, or use these distances to infer the distance to
#' group centroids.
#'
#' @docType package
#' @name usedist
NULL

#' Set the names/labels of a `"dist"` object.
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param nm New labels for the rows/columns.
#' @export
#' @examples
#' m4 <- matrix(1:16, nrow=4, dimnames=list(LETTERS[1:4]))
#' dm4 <- dist(m4)
#' dist_setNames(dm4, LETTERS[9:12])
dist_setNames <- function (d, nm) {
  # Convert to matrix so errors are generated on assignment
  # if nm does not contain the same number of elements as d
  dm <- as.matrix(d)
  dimnames(dm) <- list(nm, nm)
  stats::as.dist(dm)
}

#' Retrieve distances from a `"dist"` object.
#' Check if square
#' Check if numeric
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param idx1,idx2 Indicies specifying the distances to extract.
#' @return A vector of distances.
#' @export
#' @examples
#' m4 <- matrix(1:16, nrow=4, dimnames=list(LETTERS[1:4]))
#' dm4 <- dist(m4)
#' dist_get(dm4, "A", "C")
#' dist_get(dm4, "A", c("A", "B", "C", "D"))
#' dist_get(dm4, c("A", "B", "C"), c("B", "D", "B"))
dist_get <- function (d, idx1, idx2) {
  d <- stats::as.dist(d)
  if (is.character(idx1)) {
    idx1 <- match(idx1, attr(d, "Labels"))
  }
  if (is.character(idx2)) {
    idx2 <- match(idx2, attr(d, "Labels"))
  }
  n <- attr(d, "Size")
  if (any(is.na(idx1) | (idx1 < 1) | (idx1 > n))) {
    stop("idx1 out of range")
  }
  if (any(is.na(idx2) | (idx2 < 1) | (idx2 > n))) {
    stop("idx2 out of range")
  }
  i <- pmin(idx1, idx2)
  j <- pmax(idx1, idx2)
  # Zeros are eliminated from index vectors
  # Need to fill with NA if i and j are equal
  idx <- ifelse(i == j, NA, n*(i-1) - i*(i-1)/2 + j-i)
  ifelse(i == j, 0, d[idx])
}

#' Extract parts of a `"dist"` object.
#'
#' This function also works to re-arrange the elements of a distance matrix, if
#' the indicies are provided in the desired order.
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param idx Indices specifying the subset of distances to extract.
#' @return A distance matrix.
#' @export
#' @examples
#' m4 <- matrix(1:16, nrow=4, dimnames=list(LETTERS[1:4]))
#' dm4 <- dist(m4)
#' dist_subset(dm4, c("A", "B", "C"))
#' dist_subset(dm4, c("D", "C", "B", "A"))
dist_subset <- function (d, idx) {
  stats::as.dist(as.matrix(d)[idx, idx])
}

#' Create a data frame of distances between groups of items.
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param g A factor representing the groups of objects in `d`.
#' @return A data frame with 6 columns. "Item1" and "Item2" identify the
#'   items compared, using the label if available. Likewise, "Group1" and
#'   "Group2" identify the groups of the items. "Label" is a factor giving a
#'   convenient label for the type of comparison. Finally, "Distance" contains
#'   the distance of interest.
#' @export
#' @examples
#' m4 <- matrix(1:16, nrow=4, dimnames=list(LETTERS[1:4]))
#' dm4 <- dist(m4)
#' g4 <- rep(c("Control", "Treatment"), each=2)
#' dist_groups(dm4, g4)
dist_groups <- function(d, g) {
  d <- stats::as.dist(d)
  g <- as.factor(g)
  dsize <- attr(d, "Size")
  if (length(g) != dsize) {
    stop(
      "Length of grouping vector (g) must equal number of observations in ",
      "dist object (d)")
  }
  dlabels <- attr(d, "Labels")
  idxs <- utils::combn(dsize, 2)
  idx1 <- idxs[1,]
  idx2 <- idxs[2,]

  # For the between group labels, we need to keep the groups in factor order.
  # Here, we record the level of the group to use for the first and second
  # parts of the label.
  level1 <- levels(g)[pmin(as.numeric(g[idx1]), as.numeric(g[idx2]))]
  level2 <- levels(g)[pmax(as.numeric(g[idx1]), as.numeric(g[idx2]))]

  data.frame(
    Item1 = if (is.null(dlabels)) idx1 else dlabels[idx1],
    Item2 = if (is.null(dlabels)) idx2 else dlabels[idx2],
    Group1 = g[idx1],
    Group2 = g[idx2],
    Label = ifelse(
      level1 == level2,
      paste("Within", level1),
      paste("Between", level1, "and", level2)),
    Distance = dist_get(d, idx1, idx2))
}

#' Make a distance matrix using a custom distance function
#'
#' @param x A matrix of observations, one per row
#' @param distance_fcn A function of two arguments, used to compute the
#'   distance between two rows of the data matrix.
#' @param method Name for the distance method.  If provided, will be stored in
#'   the `"method"` attribute of the result.
#' @return A `"dist"` object containing the distances between rows of the data
#'   matrix.
#' @export
#' @examples
#' x <- matrix(sin(1:30), nrow=5)
#' rownames(x) <- LETTERS[1:5]
#' manhattan_distance <- function (v1, v2) sum(abs(v1 - v2))
#' dist_make(x, manhattan_distance, "Manhattan (custom)")
dist_make <- function (x, distance_fcn, method=NULL) {
  distance_from_idxs <- function (idxs) {
    i1 <- idxs[1]
    i2 <- idxs[2]
    distance_fcn(x[i1,], x[i2,])
  }
  size <- nrow(x)
  d <- apply(utils::combn(size, 2), 2, distance_from_idxs)
  attr(d, "Size") <- size
  xnames <- rownames(x)
  if (!is.null(xnames)) {
    attr(d, "Labels") <- xnames
  }
  attr(d, "Diag") <- FALSE
  attr(d, "Upper") <- FALSE
  if (!is.null(method)) {
    attr(d, "method") <- method
  }
  class(d) <- "dist"
  d
}

#' Compute distances from each item to group centroids
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param g A factor representing the groups of items in `d`.
#' @return A data frame with distances to the group centroids (see details).
#'
#' This function computes the distance from each item to the centroid positions
#' of groups defined in the argument `g`.  This is accomplished without
#' determining the centroid positions directly; see the documentation for
#' \code{\link{dist_between_centroids}} for details on this procedure.
#'
#' The result is a data frame with three columns:
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
#' @export
dist_to_centroids <- function (d, g) {
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
    sqrt(term12 - term1)
  }
  df$CentroidDistance <- mapply(
    dist_to_group_centroid, df$Item, df$CentroidGroup)
  df
}

#' Compute the distance between group centroids
#'
#' @param d A distance matrix object of class `"dist"`.
#' @param idx1 A vector of items in group 1
#' @param idx2 A vector of items in group 2
#' @return The distance between group centroids (see details).
#'
#' It is possible to infer the distance between group centroids directly from
#' the distances between items in each group.  The `adonis` test in the ecology
#' package `vegan` takes advantage of this approach to carry out an ANOVA-like
#' test on distances.
#'
#' The approach rests on the assumption that the items in the distance matrix
#' occupy some high-dimensional Euclidean space.  However, we do not have to
#' actually create the space to find the distance between centroids.  Using the
#' assumption that such a space exists, we can use an algebraic formula to find
#' the centroid distance.
#'
#' The formulas for this were presented by Apostol and Mnatsakanian in 2003,
#' though we need to re-arrange equation 28 to get the value we want:
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
#' @references Apostol, T.M. and Mnatsakanian, M.A. Sums of squares of distances
#'   in m-space. Math. Assoc. Am. Monthly 110, 516 (2003).
#'
#' @export
dist_between_centroids <- function (d, idx1, idx2) {
  d2 <- d ** 2
  # Should this function supprot boolean indexing? Check to see if dist_subset
  # supports booleans.
  n1 <- length(idx1)
  n2 <- length(idx2)
  sum1 <- sum(dist_subset(d2, idx1))
  sum2 <- sum(dist_subset(d2, idx2))
  sum12 <- sum(as.matrix(d2)[idx1, idx2])
  term1 <- sum1 / (n1 ** 2)
  term2 <- sum2 / (n2 ** 2)
  term12 <- sum12 / (n1 * n2)
  sqrt(term12 - term1 - term2)
}
