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

# Last change: 2021-02-25 / Llc


wrap_prepadata_ <- function(tl_inp) {

  # Register of rents

  tl_prepa_rr <- mod_prepa_rr(
    IND_YEARLY_RR = tl_inp$IND_YEARLY_RR
  )

  # Training and validation sets

  tl_mod_tsvs <- mod_tsvs(
    RR_OASI = tl_prepa_rr$RR_OASI,
    PARAM_GLOBAL = tl_inp$PARAM_GLOBAL
  )

  # Full datasets, training and validation sets of categorical and continuous
  # variables

  tl_prepa_rr <- mod_catcontvar(
    RR_OASI = tl_prepa_rr$RR_OASI,
    RR_OASI_TS = tl_mod_tsvs$RR_OASI_TS,
    RR_OASI_VS = tl_mod_tsvs$RR_OASI_VS
  )

  # Output
  c(tl_prepa_rr)
}


#' @title wrap_prepadata (memoised)
#' @export
wrap_prepadata <- memoise::memoise(wrap_prepadata_)
