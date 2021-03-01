#' @title Wrapper for the clusters construction
#'
#' @description This wrapper contains all the necessary modules which allow to
#' construct the clusters
#'
#' @param tl_inp List of input data frames.
#' @param tl_prepadata List of data frames prepared in a first step.
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_mod_calc_kamila`
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#'
#' @export

# Last change: 2021-02-23 / Llc

wrap_computation_kamila_ <- function(tl_inp,
                                     tl_prepadata) {

  # Computation
  tl_mod_calc_kamila <- mod_calc_kamila(
    PARAM_GLOBAL = tl_inp$PARAM_GLOBAL,
    CATEG_DF_TS = tl_prepadata$CATEG_DF_TS,
    CONT_DF_TS = tl_prepadata$CONT_DF_TS,
    # CATEG_DF_VS = tl_prepadata$CATEG_DF_VS,
    # CONT_DF_VS = tl_prepadata$CONT_DF_VS,
    CONT_DF = tl_prepadata$CONT_DF,
    CATEG_DF = tl_prepadata$CATEG_DF
  )

  # Output
  c(tl_mod_calc_kamila)
}


#' @title wrap_computation_kamila (memoised)
#' @export
wrap_computation_kamila <- memoise::memoise(wrap_computation_kamila_)
