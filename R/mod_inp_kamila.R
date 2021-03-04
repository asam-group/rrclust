#' @title Function to read the data
#'
#' @description Function to read the data.
#'
#' @param path path of the set of parameters set to a particular output.
#' @param list path where the outputs are saved.
#' @param method_name name of the clustering method.
#' @return tl_inp tidylist of inputs
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export

# Last change: 2021-03-04 / Llc

mod_inp_kamila <- function(path,
                           list = NULL,
                           method_name = "kamila") {

  # Parameter path
  path_parameter <- path_param(path)

  # Specific parameter path method
  path_param_meth <- file.path(path_parameter, method_name = "kamila")

  # Input folder path
  path_data <- path_data(path)

  # Inputs of csv files for all clustering methods
  path_inp_all <- file.path(path_data, method_name = "all")

  # Inputs of csv files for the specific clustering method
  path_inp_meth <- file.path(path_data, method_name = "kamila")

  # Global parameter file
  tl_PARAM_GLOBAL <- param_tidylist_read(path_parameter)
  tl_PARAM_GLOBAL$PARAM_GLOBAL$identifier_number <- clustmeth_identifier_number(
    method_name = method_name,
    path = path
  )
  tl_PARAM_GLOBAL$path_param_folder <- path

  # Specific method parameter file
  tl_PARAM_KAMILA <- param_tidylist_read(path_param_meth)
  tl_PARAM_KAMILA$PARAM_KAMILA$path_inp_kmres <- path_inp_meth

  tl_inp <- c(
    tidylist_read(path_inp_all),
    tidylist_read(path_inp_meth),
    tl_PARAM_GLOBAL,
    tl_PARAM_KAMILA
  )
}
