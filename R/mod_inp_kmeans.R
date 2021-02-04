#' @title Function to read the data
#'
#' @description Function to read the data.
#'
#' @param path path of the set of parameters set to a particular output.
#' @param path_out path where the outputs are saved.
#' @param param_replace parameter which can be used to test some other parameters.
#' @return
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export

# - Last change: 2021-02-04 / Llc

mod_inp_kmeans <- function(path, list = NULL, method_name = "kmeans"){
browser()
  path_param <- path
  path_data <- path_data(path)

  tl_PARAM_GLOBAL <- param_tidylist_read(path_param)
  tl_PARAM_GLOBAL$PARAM_GLOBAL$identifier_number <- ffh_identifier_number(vz = method_name,
                                                                          path = path)
}
