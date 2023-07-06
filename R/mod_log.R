#' Write the version of the packages
#'
#' Write the version of the rrclust and dplyr packages and the time of the
#' output production.
#'
#' @param list List of input data frames.
#' @return `LOG` Tibble with information about time and packages version.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
mod_log <- function(list = NULL) {
  LOG <- tibble(
    rrclust_version = as.character(packageVersion("rrclust")),
    dplyr_version = as.character(packageVersion("dplyr")),
    runtime = as.character(Sys.time())
  )
  mod_return(LOG)
}
