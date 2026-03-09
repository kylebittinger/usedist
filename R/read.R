#' Read a distance matrix file
#' @param file The file path of the distance matrix to be read
#' @param ... Additional arguments passed to \code{read.table}
#' @return A dist object
#' @details
#' Values in the dist object are taken from the lower triangle of the matrix.
#' @export
dist_read <- function (file, ...) {
  stats::as.dist(utils::read.table(file, ...))
}

#' Write a distance matrix file
#' @param d A dist object
#' @param file The file path to write
#' @return Returns the dist object, invisibly
#' @export
dist_write <- function(d, file) {
  d_matrix <- as.matrix(d)
  d_header <- paste0("\t", paste(colnames(d_matrix), collapse = "\t"))
  d_rows_values <- apply(d_matrix, 2, paste, collapse = "\t")
  d_rows <- paste(rownames(d_matrix), d_rows_values, sep = "\t")
  writeLines(c(d_header, d_rows), con = file)
  invisible(d)
}
