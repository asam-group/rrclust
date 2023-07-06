#' Run the Kamila estimation
#'
#' This function executes the computations of the Kamila clustering method
#' according to the parameters which are read in a first step. Eventually, the
#' outputs are written into csv or excel files.
#'
#' @param path Path of the set of parameters set to a particular output.
#' @param path_out Path where the outputs are saved.
#' @param param_replace Parameter which can be used to test some other
#'   parameters.
#' @export
run_kamila <- function(path,
                       path_out = file.path(tempdir(), "out"),
                       param_replace = NULL) {
  if (length(path) > 1) {
    return(multi_run(
      path = path,
      fun = run_kamila
    ))
  }

  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila") |>
    param_replace(param_replace = param_replace)

  tl_out_kamila <- wrap_kamila(tl_inp_kamila = tl_inp_kamila)

  path_out_identifier <- mod_out_kamila(
    path = path,
    path_out = path_out,
    tl_inp_kamila = tl_inp_kamila,
    tl_out_kamila = tl_out_kamila
  )

  tidylist_write(
    c(tl_out_kamila, mod_log()),
    path_out_identifier
  )

  copy_param(path, path_out_identifier)

  path_out_identifier
}
