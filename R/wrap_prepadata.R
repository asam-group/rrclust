#' Prepare the data
#'
#' This wrapper contains all the necessary modules which allow to
#' prepare the data.
#'
#' @param tl_inp List of input data frames.
#'
#' @return `tl_mod_catcontvar`: A `tidylist` containing the following data frames:
#'   - `CATEG_DF`: Contains only categorical variables (factors).
#'   - `CONT_DF` : Contains only continuous variables (numeric).
#'   - `CATEG_DF_TS`: Contains only categorical variables (factors),
#'   training set.
#'   - `CONT_DF_TS` : Contains only continuous variables (numeric),
#'   training set.
#'   - `CATEG_DF_VS`: Contains only categorical variables (factors),
#'    validation set.
#'   - `CONT_DF_VS` : Contains only continuous variables (numeric),
#'   validation set.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_prepadata_ <- function(tl_inp) {
  tl_prepa_rr <- mod_prepa_rr(
    IND_YEARLY_RR = tl_inp$IND_YEARLY_RR
  )

  tl_mod_tsvs <- mod_tsvs(
    RR_OASI = tl_prepa_rr$RR_OASI,
    PARAM_GLOBAL = tl_inp$PARAM_GLOBAL
  )

  tl_mod_catcontvar <- mod_catcontvar(
    RR_OASI = tl_prepa_rr$RR_OASI,
    RR_OASI_TS = tl_mod_tsvs$RR_OASI_TS,
    RR_OASI_VS = tl_mod_tsvs$RR_OASI_VS,
    PARAM_GLOBAL = tl_inp$PARAM_GLOBAL
  )

  c(tl_mod_catcontvar)
}


#' Memoise a copy of \code{\link{wrap_prepadata}}
#' 
#' Create a memoised copy of \code{\link{wrap_prepadata}}.
#' 
#' @param tl_inp List of input data frames.
#' 
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_prepadata <- memoise::memoise(wrap_prepadata_)
