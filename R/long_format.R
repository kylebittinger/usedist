check_packages <- function (fcn_name, pkg_names) {
  is_installed <- sapply(pkg_names, requireNamespace, quietly = TRUE)
  pkgs_not_installed <- pkg_names[!is_installed]
  if (length(pkgs_not_installed) == 1) {
    msg <- paste0(
      "Package \"", pkgs_not_installed, "\" is not installed, but is ",
      "needed by the function \"", fcn_name, "\".  Please install the ",
      "missing package to use this function."
    )
    stop(msg, call. = FALSE)
  } else if (length(pkgs_not_installed) > 1) {
    pkgs_not_installed <- paste(pkgs_not_installed, collapse = "\", \"")
    msg <- paste0(
      "Packages \"", pkgs_not_installed, "\" are not installed, but are ",
      "needed by the function \"", fcn_name, "\".  Please install the ",
      "missing packages to use this function."
    )
    stop(msg, call. = FALSE)
  }
}

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
  check_packages(
    "usedist::pivot_to_numeric_matrix",
    c("dplyr", "tidyr", "tibble"))
  obs_col <- dplyr::enquo(obs_col)
  feature_col <- dplyr::enquo(feature_col)
  value_col <- dplyr::enquo(value_col)
  value_fill <- list(0)
  names(value_fill) <- dplyr::as_label(value_col)
  data_wide <- tidyr::pivot_wider(
    data,
    id_cols = !!obs_col,
    names_from = !!feature_col,
    values_from = !!value_col,
    values_fill = value_fill)
  data_wide <- tibble::column_to_rownames(data_wide, dplyr::as_label(obs_col))
  as.matrix(as.data.frame(data_wide))
}

#' Compute distances on data in long format
#'
#' @param data A data frame with numerical values in long format.
#' @param obs_col The column listing the observation, or row of the matrix.
#' @param feature_col The column listing the feature, or column of the matrix.
#' @param value_col The column listing the value, to be placed inside the
#'   matrix.
#' @param distance_fcn The distance function to use. This function should take
#'   two numeric vectors and return a number representing the distance or
#'   dissimilarity.
#'
#'#' This function requires the packages \code{dplyr}, \code{tibble}, and
#' \code{tidyr} to be installed. If they are not installed, the function will
#' generate an error, with a message to install the appropriate packages.
#'
#' @export
#' @examples
#' longdata <- expand.grid(
#'   SampleID = c("A", "B", "C"),
#'   FeatureID = c("x", "y", "z"))
#' longdata$Value = sin(1:9)
#' longdata
#'
#' manhattan_distance <- function (v1, v2) sum(abs(v1 - v2))
#' dist_long(longdata, SampleID, FeatureID, Value, manhattan_distance)
dist_long <- function (data, obs_col, feature_col, value_col, distance_fcn) {
  check_packages("usedist::dist_long", c("dplyr", "tibble", "tidyr"))
  obs_col <- dplyr::enquo(obs_col)
  feature_col <- dplyr::enquo(feature_col)
  value_col <- dplyr::enquo(value_col)
  data_matrix <- pivot_to_numeric_matrix(data, !!obs_col, !!feature_col, !!value_col)
  usedist::dist_make(data_matrix, distance_fcn)
}
