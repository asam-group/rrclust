#' @title Function writing the packages version used to run this output
#' @description Writes the version of the rrclust and dplyr packages and the
#' time of the output production.
#' @param list tidylist
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @return `LOG` tibble with information about time and packages version.
#' @export
mod_log <- function(list = NULL) {
  LOG <- tibble(
    rrclust_version = as.character(packageVersion("rrclust")),
    dplyr_version = as.character(packageVersion("dplyr")),
    runtime = as.character(Sys.time())
  )

  mod_return(LOG)
}
