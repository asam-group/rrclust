#' Resolve the path_out
#'
#' Writes the path_out and the unique identifier_number of the output.
#'
#' @param path_out Directory to store the output folder.
#' @param identifier_number Unique identifier number to name the output folder.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @return Path_out directory to store the output.
#' @export
path_out_resolve <- function(path_out,
                             identifier_number) {
  path_out <- file.path(path_out, identifier_number)
  dir_create(path_out)
  dir_create(file.path(path_out, "post_process"))
  path_out
}
