#' @title Function to read the data, to execute the computations of the Kamila
#' clustering method and write the data and the parameters (of the inputs and
#' the outputs).
#'
#' @description This function executes the computations of the Kamila clustering
#'  method according to the parameters which are read in a first step. In a
#'  third step, the outputs are delivered.
#'
#' @param path path of the set of parameters set to a particular output.
#' @param path_out path where the outputs are saved.
#' @param param_replace parameter which can be used to test some other parameters.
#'
#' @export

# - Last change: 2021-02-28 / Llc

run_kamila <- function(path,
                       path_out = file.path(tempdir(), "out"),
                       param_replace = NULL) {

  # batch run
  if (length(path) > 1) {
    return(multi_run(
      path = path,
      fun = run_kamila
    ))
  }

  # input
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila") %>%
    param_replace(param_replace = param_replace)

  # computations
  tl_out_kamila <- wrap_kamila(tl_inp_kamila = tl_inp_kamila)

  # output
  path_out_identifier <- mod_out_kamila(
    path = path,
    path_out = path_out,
    tl_inp_kamila = tl_inp_kamila,
    tl_out_kamila = tl_out_kamila
  )

  # CSV
  tidylist_write(
    c(tl_out_kamila, mod_log()),
    path_out_identifier
  )

  # Excel
  # ...

  # Store params (for reference)
  copy_param(path, path_out_identifier)

  path_out_identifier
}
