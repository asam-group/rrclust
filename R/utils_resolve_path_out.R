#' @title Function resolving the path_out
#' @description Writes the path_out and the unique identifier_number of the output.
#' @param path_out directory to store the output folder.
#' @param identifier_number unique identifier number to name the output folder,
#' defined in the function \code{\link{(mod_inp_kamila}}.
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#' @return path_out directory to store the output
#' @export

# Last change: 2021-03-01 / Llc
path_out_resolve <- function(path_out,
                             identifier_number) {
  path_out <- file.path(path_out, identifier_number)
  fs::dir_create(path_out)
  fs::dir_create(file.path(path_out, "post_process"))
  path_out
}
