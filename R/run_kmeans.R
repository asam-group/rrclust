#' @title Function to read the data, to execute the computations of the K-means
#' clustering method and write the data and the parameters (of the inputs and
#' the outputs).
#'
#' @description This function executes the computations of the K-means clustering
#'  method according to the parameters which are read in a first step. In a
#'  third step, the outputs are delivered.
#'
#' @param path path of the set of parameters set to a particular output.
#' @param path_out path where the outputs are saved.
#' @param param_replace parameter which can be used to test some other parameters.
#'
#' @export

# - Last change: 2021-02-04 / Llc

run_kmeans <- function(path, path_out = file.path(tempdir(), "out"),
                       param_replace = NULL) {

  # batch run
  if (length(path) > 1) {
    return(multi_run(
      path = path,
      fun = run_kmeans
    ))
  }

  # input
  tl_inp_kmeans <- mod_inp_kmeans(path = path, method_name = "kmeans") %>%
    param_replace(param_replace = param_replace)

  # computations
  tl_out_kmeans <- wrap_kmeans(tl_inp_kmeans = tl_inp_kmeans)

  # output
  path_out_identifier <- mod_out_kmeans(
    path = path,
    path_out = path_out,
    tl_inp_kmeans = tl_inp_kmeans,
    tl_out_kmeans = tl_out_kmeans
  )

  # CSV
  tidylist_write(c(tl_out_kmeans, mod_log()), path_out_identifier)

  # Excel
  # ...

  # Store params (for reference)
  copy_param(path, path_out_identifier)

  path_out_identifier
}
