#' @title Function writing the packages version used to run this output
#' @description Writes the version of the rrclust and dplyr packages and the time
#' of the output production.
#' @param list tidylist
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#' @return LOG tibble with information about time and packages version.
#' @export

# Last change: 2021-03-01 / Llc

mod_log <- function(list = NULL) {
  LOG <- tibble(
    rrclust_version = as.character(packageVersion("rrclust")),
    dplyr_version = as.character(packageVersion("dplyr")),
    runtime = as.character(Sys.time())
  )

  mod_return(LOG)
}
