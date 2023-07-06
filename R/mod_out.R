#' Output Kamila
#'
#' @description Saves the Kamila output
#'
#' @param path Vector containing container path.
#'
#' @param path_out Vector containing output path.
#'
#' @param tl_inp_kamila List containing input data Kamila.
#'
#' @param tl_out_kamila List containing output data Kamila.
#'
#' @return `path_out_identifier`: Path to output container.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#'
#' @export
mod_out_kamila <- function(path,
                           path_out,
                           tl_inp_kamila,
                           tl_out_kamila) {
  path_out_identifier <-
    path_out_resolve(
      path_out,
      tl_inp_kamila$PARAM_GLOBAL$identifier_number
    )

  tidylist_write(c(tl_out_kamila, mod_log()), path_out_identifier)

  copy_param(path, path_out_identifier)

  return(path_out_identifier)
}
