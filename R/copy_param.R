#' @title Function copying the description and the input path from files to the
#' PARAM_GLOBAL
#' @description Writes the description and the input path from text files to the
#' PARAM_GLOBAL.
#' @param path_out directory to store the output folder.
#' @param path path of the parameter folder.
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export

# Last change: 2021-03-01 / Llc
#
copy_param <- function(path,
                       path_out) {
  fs::dir_copy(path_param(path), file.path(path_out, "param"))

  # legacy mode, substitute PARAM_GLOBAL by one that contains description and input path
  if (check_container_dir(path)) {
    DESCRIPTION <- read_description(path)
    PARAM_GLOBAL <- param_tidylist_read(path_param(path))$PARAM_GLOBAL
    PARAM_GLOBAL <- bind_cols(PARAM_GLOBAL, DESCRIPTION)
    PARAM_GLOBAL$path_data <- read_path_data(path)
    write_param(PARAM_GLOBAL, file = file.path(path_out, "param", "PARAM_GLOBAL.csv"))
  }
}
