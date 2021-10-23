#' Convert a data frame in long format to a numeric matrix
#'
#' @param data A data frame with numerical values in long format.
#' @param obs_col The column listing the observation, or row of the matrix.
#' @param feature_col The column listing the feature, or column of the matrix.
#' @param value_col The column listing the value, to be placed inside the
#'   matrix.
#'
#' The parameters \code{obs_col}, \code{feature_col}, and \code{value_col}
#' should be provided as bare column names. If any combination of row and
#' column does not appear in the data frame, a zero will be entered in the
#' resultant matrix.
#'
#' This function requires the packages \code{dplyr}, \code{tibble}, and
#' \code{tidyr} to be installed. If they are not installed, the function will
#' generate an error, with a message to install the appropriate packages.
#'
#' @export
#' @examples
#' longdata <- data.frame(
#'   SampleID = paste0("Sample", c(1, 1, 1, 2, 2, 3, 3)),
#'   FeatureID = paste0("Feature", c(1, 2, 3, 1, 2, 2, 3)),
#'   Value = c(132, 41, 7, 56, 11, 929, 83))
#' longdata
#' pivot_to_numeric_matrix(longdata, SampleID, FeatureID, Value)
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
