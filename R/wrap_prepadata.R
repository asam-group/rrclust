#' @title Wrapper for the preparation of the data
#'
#' @description This wrapper contains all the necessary modules which allow to
#' prepare the data.
#'
#' @param tl_inp List of input data frames.
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_prepa_rr`
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#'
#' @export

# Last change: 2021-02-11 / Llc


wrap_prepadata_ <- function(tl_inp) {

  # Register of rents

  tl_prepa_rr <- mod_prepa_rr(
    IND_YEARLY_RR = tl_inp$IND_YEARLY_RR
  )

  # Output
  c(tl_prepa_rr)
}


#' @title wrap_prepadata (memoised)
#' @export
wrap_prepadata <- memoise::memoise(wrap_prepadata_)
