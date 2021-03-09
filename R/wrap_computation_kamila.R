#' @title Wrapper for the clusters construction
#'
#' @description This wrapper contains all the necessary modules which allow to
#' construct the clusters
#'
#' @param tl_inp List of input data frames of which we use:
#' - `PARAM_KAMILA$calc_gstar`: If TRUE, estimates the clusters. Else, takes the
#' parameter PARAM_KAMILA$param_gstar.
#' @param tl_prepadata List of data frames prepared in a first step.
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_mod_calc_kamila`
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.pipoz@bsv.admin.ch)
#'
#' @export

# Last change: 2021-03-04 / Llc

wrap_computation_kamila_ <- function(tl_inp,
                                     tl_prepadata) {


  #Exclude the response variable aadr from the TS, VS and the full dataset
  if (tl_inp$PARAM_KAMILA$exclude_aadr){
    CONT_DF_TS <- tl_prepadata$CONT_DF_TS %>%
      dplyr::select(-aadr)

    CONT_DF_VS <- tl_prepadata$CONT_DF_VS %>%
      dplyr::select(-aadr)

    CONT_DF <- tl_prepadata$CONT_DF %>%
      dplyr::select(-aadr)

  } else {
    CONT_DF_TS <- tl_prepadata$CONT_DF_TS

    CONT_DF_VS <- tl_prepadata$CONT_DF_VS

    CONT_DF <- tl_prepadata$CONT_DF
  }

  # Run the algorithm on the TS to find the optimal number of clusters gstar
  # Writes gstar as the parameter PARAM_KAMILA$param_gstar
  if (tl_inp$PARAM_KAMILA$calc_gstar) {
    tl_mod_gstar <- mod_gstar(
      PARAM_KAMILA = tl_inp$PARAM_KAMILA,
      CATEG_DF_TS = tl_prepadata$CATEG_DF_TS,
      CONT_DF_TS = CONT_DF_TS
    )
  }

  # PARAM_KAMILA with the updated gstar parameter
  PARAM_KAMILA <- if (tl_inp$PARAM_KAMILA$calc_gstar) {
    tl_mod_gstar$PARAM_KAMILA
  } else {
    tl_inp$PARAM_KAMILA
  }

  # Apply gstar to the whole dataset
  tl_mod_calc_kamila <- mod_calc_kamila(
    PARAM_KAMILA = PARAM_KAMILA,
    CONT_DF = CONT_DF,
    FULL_CONT_DF = tl_prepadata$CONT_DF,
    CATEG_DF = tl_prepadata$CATEG_DF
  )

  # Output
  if (tl_inp$PARAM_KAMILA$calc_gstar) {
    c(
      tl_mod_calc_kamila,
      tl_mod_gstar
    )
  } else {
    c(tl_mod_calc_kamila)
  }
}


#' @title wrap_computation_kamila (memoised)
#' @export
wrap_computation_kamila <- memoise::memoise(wrap_computation_kamila_)
