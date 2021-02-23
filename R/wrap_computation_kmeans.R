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

wrap_computation_kamila_ <- function(tl_inp,
                                     tl_prepadata) {

  # Computation
  tl_mod_calc_kamila <- mod_calc_kamila(
    PARAM_GLOBAL = tl_inp$PARAM_GLOBAL,
    CATEG_DF = tl_prepadata$CATEG_DF,
    CONT_DF = tl_prepadata$CONT_DF
  )

  # Output
  c(tl_mod_calc_kamila)
}


#' @title wrap_computation_kamila (memoised)
#' @export
wrap_computation_kamila <- memoise::memoise(wrap_computation_kamila_)
