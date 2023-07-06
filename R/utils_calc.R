#' Standardize continuous variables
#'
#' Function to standardize continuous variables.
#'
#' @param x Variable to standardize
#' @export
rangeStandardize <- function(x) {
  (x - min(x)) / diff(range(x))
}
