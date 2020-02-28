#' usedist: a package for working with distance matrices in R
#'
#' In usedist, we provide a number of functions to help with distance matrix
#' objects, such as those produced by the \code{dist} function.  Some functions
#' are geared towards making or altering distance matrix objects.  Others
#' relate to groups of items in the distance matrix. They provide access to
#' within- or between-group distances, or use these distances to infer the
#' distance to group centroids.
#'
#' @docType package
#' @name usedist
NULL

#' Set the names/labels of a \code{dist} object.
#'
#' @param d A distance matrix object of class \code{dist}.
#' @param nm New labels for the rows/columns.
#' @return A distance matrix with new row/column labels.
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

#' Retrieve distances from a \code{dist} object.
#'
#' @param d A distance matrix object of class \code{dist}.
#' @param idx1,idx2 Indices specifying the distances to extract.
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

#' Extract parts of a \code{dist} object.
#'
#' Extract a subset of values from a distance matrix. This function also works
#' to re-arrange the rows of a distance matrix, if they are provided in the
#' desired order.
#'
#' @param d A distance matrix object of class \code{dist}.
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
#' @param d A distance matrix object of class \code{dist}.
#' @param g A factor representing the groups of objects in \code{d}.
#' @return A data frame with 6 columns:
#' \describe{
#'   \item{Item1, Item2}{The items being compared.}
#'   \item{Group1, Group2}{The groups to which the items belong.}
#'   \item{Label}{A convenient label for plotting or comparison.}
#'   \item{Distance}{The distance between Item1 and Item2.}}
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
    Label = factor(ifelse(
      level1 == level2,
      paste("Within", level1),
      paste("Between", level1, "and", level2))),
    Distance = dist_get(d, idx1, idx2),
    stringsAsFactors = FALSE)
}

#' Make a distance matrix using a custom distance function
#'
#' @param x A matrix of observations, one per row
#' @param distance_fcn A function used to compute the distance between two
#'   rows of the data matrix.  The two rows will be passed as the first and
#'   second arguments to \code{distance_fcn}.
#' @param ... Additional arguments passed to \code{distance_fcn}.
#' @return A \code{dist} object containing the distances between rows of the
#'   data matrix.
#' @details We do not set the \code{call} or \code{method} attributes of the
#'   \code{dist} object.
#' @export
#' @examples
#' x <- matrix(sin(1:30), nrow=5)
#' rownames(x) <- LETTERS[1:5]
#' manhattan_distance <- function (v1, v2) sum(abs(v1 - v2))
#' dist_make(x, manhattan_distance)
dist_make <- function (x, distance_fcn, ...) {
  distance_from_idxs <- function (idxs) {
    i1 <- idxs[1]
    i2 <- idxs[2]
    distance_fcn(x[i1,], x[i2,], ...)
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
  class(d) <- "dist"
  d
}
