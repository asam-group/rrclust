#' @title Preparation of the kamila results of the training set.
#'
#' @description Prepares the kamila results of the training set and saves the
#' best number of clusters gstar.
#'
#' @param PARAM_INPUTS a data frame containing the data paths.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `KM_RES` : contains the kamila results of the training set.
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# - `Last change`: 2021-03-03 / Llc

mod_input_kmres <- function(PARAM_INPUTS,
                            list = NULL) {
  mod_init()

  KM_RES <- load(file = file.path(
    PARAM_INPUTS$path_kmres,
    PARAM_INPUTS$file_kmres
  ))

  mod_return(KM_RES)
}
