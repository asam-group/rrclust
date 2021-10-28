#' @title Output Kamila
#'
#' @description Saves the Kamila output
#'
#' @param path vector containing container path.
#'
#' @param path_out vector containing output path.
#'
#' @param tl_inp_kamila list containing input data Kamila.
#'
#' @param tl_out_kamila list containing output data Kamila.
#'
#' @return path_out_identifier path to output container
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# Last change: 2021-02-23 / Llc


mod_out_kamila <- function(path,
                           path_out,
                           tl_inp_kamila,
                           tl_out_kamila) {

  # construct output path
  path_out_identifier <-
    path_out_resolve(
      path_out,
      tl_inp_kamila$PARAM_GLOBAL$identifier_number
    )

  # write csv
  tidylist_write(c(tl_out_kamila, mod_log()), path_out_identifier)

  # Store params
  copy_param(path, path_out_identifier)

  return(path_out_identifier)
}
