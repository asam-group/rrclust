#' @title Wrapper for the clusters construction.
#'
#' @description This wrapper contains all the necessary modules which allow to
#' construct the clusters.
#'
#' @param tl_inp List of input data frames of which we use:
#' - `PARAM_KAMILA$calc_kstar`: If TRUE, estimates the clusters. Else, takes the
#' parameter PARAM_KAMILA$param_kstar.
#'
#' @param tl_prepadata List of data frames prepared in a first step.
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_mod_calc_kamila`
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#'
#' @export

# Last change: 2021-03-11 / Llc

wrap_computation_kamila_ <- function(tl_inp,
                                     tl_prepadata) {


  # Exclude the response variables aadr and monthly_rent from the TS, VS and the
  # full dataset
  CONT_DF_TS <- tl_prepadata$CONT_DF_TS %>%
    dplyr::select(
      -aadr,
      -monthly_rent
    )

  CONT_DF_VS <- tl_prepadata$CONT_DF_VS %>%
    dplyr::select(
      -aadr,
      -monthly_rent
    )

  CONT_DF <- tl_prepadata$CONT_DF %>%
    dplyr::select(
      -aadr,
      -monthly_rent
    )


  # Exclude the categorical variables marital_stat and benef_type from the TS, VS
  # and the full dataset
  CATEG_DF_TS <- tl_prepadata$CATEG_DF_TS %>%
    dplyr::select(
      -marital_stat,
      -benef_type
    )

  CATEG_DF_VS <- tl_prepadata$CATEG_DF_VS %>%
    dplyr::select(
      -marital_stat,
      -benef_type
    )

  CATEG_DF <- tl_prepadata$CATEG_DF %>%
    dplyr::select(
      -marital_stat,
      -benef_type
    )


  # Run the algorithm on the TS to find the optimal number of clusters kstar
  # Writes kstar as the parameter PARAM_KAMILA$param_kstar
  if (tl_inp$PARAM_KAMILA$calc_kstar) {
    tl_mod_kstar <- mod_kstar(
      PARAM_KAMILA = tl_inp$PARAM_KAMILA,
      CATEG_DF_TS = CATEG_DF_TS,
      CONT_DF_TS = CONT_DF_TS
    )
  }

  # PARAM_KAMILA with the updated kstar parameter
  PARAM_KAMILA <- if (tl_inp$PARAM_KAMILA$calc_kstar) {
    tl_mod_kstar$PARAM_KAMILA
  } else {
    tl_inp$PARAM_KAMILA
  }

  # Apply kstar to the whole dataset
  tl_mod_calc_kamila <- mod_calc_kamila(
    PARAM_KAMILA = PARAM_KAMILA,
    CONT_DF = CONT_DF,
    FULL_CONT_DF = tl_prepadata$CONT_DF,
    CATEG_DF = CATEG_DF,
    FULL_CATEG_DF = tl_prepadata$CATEG_DF
  )

  # Output
  if (tl_inp$PARAM_KAMILA$calc_kstar) {
    c(
      tl_mod_calc_kamila,
      tl_mod_kstar
    )
  } else {
    c(tl_mod_calc_kamila)
  }
}


#' @title wrap_computation_kamila (memoised)
#' @export
wrap_computation_kamila <- memoise::memoise(wrap_computation_kamila_)
