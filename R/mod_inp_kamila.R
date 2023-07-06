#' Function to read the data
#'
#' @description Function to read the data.
#'
#' @param path Path of the set of parameters set to a particular output.
#' @param list Path where the outputs are saved.
#' @param method_name Name of the clustering method.
#' @return tl_inp Tidylist of inputs.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
mod_inp_kamila <- function(path,
                           list = NULL,
                           method_name = "kamila") {
  path_parameter <- path_param(path)

  path_param_meth <- file.path(path_parameter, method_name = "kamila")

  path_data <- path_data(path)

  path_inp_all <- file.path(path_data, method_name = "all")

  path_inp_meth <- file.path(path_data, method_name = "kamila")

  tl_PARAM_GLOBAL <- param_tidylist_read(path_parameter)
  tl_PARAM_GLOBAL$PARAM_GLOBAL$identifier_number <- clustmeth_identifier_number(
    method_name = method_name,
    path = path
  )
  tl_PARAM_GLOBAL$path_param_folder <- path

  tl_PARAM_KAMILA <- param_tidylist_read(path_param_meth)
  tl_PARAM_KAMILA$PARAM_KAMILA$path_inp_kmres <- path_inp_meth

  tl_inp <- c(
    tidylist_read(path_inp_all),
    tidylist_read(path_inp_meth),
    tl_PARAM_GLOBAL,
    tl_PARAM_KAMILA
  )
  tl_inp
}
