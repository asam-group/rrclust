#' Function copying the description and the input path from files to the
#' PARAM_GLOBAL
#' Writes the description and the input path from text files to the
#' PARAM_GLOBAL.
#' @param path_out Directory to store the output folder.
#' @param path Path of the parameter folder.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
copy_param <- function(path,
                       path_out) {
  dir_copy(path_param(path), file.path(path_out, "param"))

  if (check_container_dir(path)) {
    DESCRIPTION <- read_description(path)
    PARAM_GLOBAL <- param_tidylist_read(path_param(path))$PARAM_GLOBAL
    PARAM_GLOBAL <- bind_cols(PARAM_GLOBAL, DESCRIPTION)
    PARAM_GLOBAL$path_data <- read_path_data(path)
    write_param(PARAM_GLOBAL, file = file.path(
      path_out,
      "param",
      "PARAM_GLOBAL.csv"
    ))
  }
}
