#' Read a distance matrix file.
#' @param file The file path of the distance matrix to be read
#' @param ... Additional arguments passed to \code{read.table}
#' @return A dist object
#' @details
#' Values in the dist object are taken from the lower triangle of the matrix.
#' @export
dist_read <- function (file, ...) {
  as.dist(read.table(file, ...))
}
