#' @title Preparation of the inputs for the calculation
#'
#' @description Reading, preparing and saving the inputs.
#'
#' @param path path to container
#'
#' @return nothing. Output (the input data) is stored in the assigned location
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# Last change: 2021-06-02 / Llc


prepare_input <- function(path,
                          path_out = file.path(dirname(path)),
                          overwrite = FALSE) {

  # Parameters -----------------------------------------------------------------

  PARAM_INPUTS <- read_param(path)

  # Paths ----------------------------------------------------------------------

  inp_path_all <- file.path(path_out, "all")

  ensure_path <- function(path) {
    # do not allow overwriting if overwrite == FALSE
    if (!overwrite && file.exists(path)) stop(path, "already exists")
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    file.remove(list.files(path, full.names = TRUE))
  }

  ensure_path(inp_path_all)

  # Input Data -----------------------------------------------------------------

  # Input register of rents

  IND_YEARLY_RR <- mod_input_ind_yearly_rr(
    PARAM_INPUTS = PARAM_INPUTS
  )$IND_YEARLY_RR


  ## Collect and store inputs --------------------------------------------------

  # Kamila
  all_inputs <- tidylist(
    IND_YEARLY_RR
  )
  tidylist_write(all_inputs, inp_path_all)
}
