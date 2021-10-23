#' Convert a data frame in long format to a matrix
#'
#' @param data A data frame in long format.
#' @param rows_from The column indicating the row of the matrix.
#' @param cols_from The column indicating the column of the matrix.
#' @param values_from The column indicating the value to be placed inside the
#'   matrix.
#' @param fill The value to use for missing combinations of rows and columns.
#' @param obs_col,feature_col,value_col The same as \code{rows_from},
#'   \code{cols_from}, and \code{values_from}, respectively.
#'
#' @details
#' The parameters \code{rows_from}, \code{cols_from}, and \code{values_from}
#' should be provided as bare column names.
#'
#' This function requires the packages \code{tidyr}, \code{rlang}, and
#' \code{tibble} to be installed. If they are not installed, the function will
#' generate an error, with a message to install the appropriate packages.
#'
#' @export
#' @examples
#' longdata <- data.frame(
#'   sample_id = paste0("Sample", c(1, 1, 1, 2, 2, 3, 3)),
#'   feature_id = paste0("Feature", c(1, 2, 3, 1, 2, 2, 3)),
#'   counts = c(132, 41, 7, 56, 11, 929, 83))
#' pivot_to_matrix(longdata, sample_id, feature_id, counts)
pivot_to_matrix <- function (data, rows_from, cols_from, values_from, fill = 0) {
  check_pkg_functions(
    c("rlang", "as_name"), c("rlang", "ensym"), c("tidyr", "pivot_wider"),
    c("tibble", "column_to_rownames"))

  values_fill <- list(fill)
  names(values_fill) <- rlang::as_name(rlang::ensym(values_from))
  data_wide <- tidyr::pivot_wider(
    data,
    id_cols = {{ rows_from }},
    names_from = {{ cols_from }},
    values_from = {{ values_from }},
    values_fill = values_fill)
  data_wide <- tibble::column_to_rownames(
    data_wide, rlang::as_name(rlang::ensym(rows_from)))
  as.matrix(as.data.frame(data_wide))
}

#' @describeIn pivot_to_matrix Specialized version for numeric values.
#'   Deprecated; use \code{pivot_to_matrix} instead.
#' @export
pivot_to_numeric_matrix <- function (data, obs_col, feature_col, value_col) {
  pivot_to_matrix(
    data, {{ obs_col }}, {{ feature_col }}, {{ value_col }}, fill = 0)
}

check_pkg_functions <- function (...) {
  to_check <- list(...)
  messages <- vapply(
    to_check, error_message_for_pkg_function, FUN.VALUE = "a")
  messages <- unique(messages[!is.na(messages)])
  if (length(messages) > 0) {
    combined_messages <- paste(
      "The following packages or functions are not available:",
      messages, sep = " ", collapse = " ")
    stop(combined_messages, call. = FALSE)
  }
}

error_message_for_pkg_function <- function (x) {
  pkg <- x[1]
  fcn <- x[2]
  pkg_is_installed <- requireNamespace(pkg, quietly = TRUE)
  if (!pkg_is_installed) {
    return(paste("Package", pkg, "is not installed."))
  }
  fcn_is_available <- exists(fcn, where=asNamespace(pkg), mode="function")
  if (!fcn_is_available) {
    return(paste(
      "Package", pkg, "is installed but function", fcn, "is not available."))
  }
  NA_character_
}
