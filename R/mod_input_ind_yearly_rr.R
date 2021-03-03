#' @title Preparation of the register of rents.
#'
#' @description Prepares and saves the register of rents.
#'
#' @param PARAM_INPUTS a data frame containing the data paths.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `IND_YEARLY_RR` : contains all the beneficiaries of the OASI
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# - `Last change`: 2021-03-03 / Llc

mod_input_ind_yearly_rr <- function(PARAM_INPUTS,
                                    list = NULL) {
  mod_init()

  IND_YEARLY_RR <- load(file = file.path(
    PARAM_INPUTS$path_rr,
    PARAM_INPUTS$file_rr
  ))

  mod_return(IND_YEARLY_RR)
}
