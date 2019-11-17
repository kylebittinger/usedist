check_tidyverse <- function () {
  pkg_names <- c("dplyr", "tidyr", "tibble")
  is_installed <- sapply(pkg_names, requireNamespace, quietly = TRUE)
  pkgs_not_installed <- pkg_names[!is_installed]
  if (length(pkgs_not_installed) == 1) {
    msg <- paste0(
      "Package \"", pkgs_not_installed, "\" is not installed, but is ",
      "needed by \"usedist::pivot_to_numeric_matrix\".  Please install the ",
      "missing package to use this function."
    )
    stop(msg, call. = FALSE)
  }
  if (length(pkgs_not_installed) > 1) {
    pkgs_not_installed <- paste(pkgs_not_installed, collapse = "\", \"")
    msg <- paste0(
      "Packages \"", pkgs_not_installed, "\" are not installed, but are ",
      "needed by \"usedist::pivot_to_numeric_matrix\".  Please install the ",
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
  check_tidyverse()
  obs_col <- dplyr::enquo(obs_col)
  feature_col <- dplyr::enquo(feature_col)
  value_col <- dplyr::enquo(value_col)
  value_fill <- list(0)
  names(value_fill) <- dplyr::as_label(value_col)
  # The function pivot_wider is not in older versions of tidyr.
  # Fall back to spread if pivot_wider is not found
  if (exists("pivot_wider", where=asNamespace("tidyr"), mode="function")) {
    data_wide <- tidyr::pivot_wider(
      data,
      id_cols = !!obs_col,
      names_from = !!feature_col,
      values_from = !!value_col,
      values_fill = value_fill)
  } else {
    data <- dplyr::select(data, !!obs_col, !!feature_col, !!value_col)
    data_wide <- tidyr::spread(data, !!feature_col, !!value_col)
  }
  data_wide <- tibble::column_to_rownames(data_wide, dplyr::as_label(obs_col))
  as.matrix(as.data.frame(data_wide))
}
