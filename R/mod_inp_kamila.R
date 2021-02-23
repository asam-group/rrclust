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

# Last change: 2021-02-11 / Llc

mod_inp_kamila <- function(path, list = NULL, method_name = "kamila") {
  path_param <- path
  path_data <- path_data(path)

  tl_PARAM_GLOBAL <- param_tidylist_read(path_param)
  tl_PARAM_GLOBAL$PARAM_GLOBAL$identifier_number <- clustmeth_identifier_number(
    method_name = method_name,
    path = path
  )

  tl_inp <- c(
    tidylist_read(path_data),
    tl_PARAM_GLOBAL
  )
}
