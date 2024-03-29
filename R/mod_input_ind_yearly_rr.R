#' Read of the pension register
#'
#' Read the pension register and save it into an tidy data frame.
#'
#' @param PARAM_INPUTS a data frame containing the data paths.
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `IND_YEARLY_RR` : contains all the beneficiaries of the OASI
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#'
#' @export

mod_input_ind_yearly_rr <- function(PARAM_INPUTS,
                                    list = NULL) {
  mod_init()

  x <- load(file = file.path(
    PARAM_INPUTS$path_rr,
    PARAM_INPUTS$file_rr
  ))

  IND_YEARLY_RR <- get(x)

  mod_return(IND_YEARLY_RR)
}
