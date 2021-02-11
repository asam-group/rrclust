#' @title Estimation of the clusters according to the K-means method.
#'
#' @description Estimation of the clusters according to the K-means method.
#'
#' @param tl_inp_kamila set of the needed output.
#' @return
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-02-11 / Llc

mod_calc_kamila <- function(PARAM_GLOBAL,
                            RR_OASI,
                            list = NULL) {
  mod_init()
browser()


  kamRes <- kamila(conDf, catDf, numClust = 2, numInit = 10)
  table(kamRes$finalMemb, dat$trueID)
  # Try Kamila method

  # # Filter
  # # Only the old age insurance beneficiaries
  # if (PARAM_GLOBAL$filter_posit_ageret) {
  #   X <- X %>%
  #     filter(age_retire >= 0) %>%
  #     dplyr::select(
  #       -benef_type1,
  #       -benef_type2,
  #       -benef_type3,
  #       -benef_type4,
  #       -benef_type5,
  #       -benef_type6,
  #       -benef_type7,
  #       -benef_type8
  #     )
  # }
  #
  # #Newly retired only
  # if (PARAM_GLOBAL$filter_newly_retired) {
  #   X <- X %>%
  #     filter(age_retire == age)
  # }
  #
  #
  # # Scaling
  # X.scale <- scale(X)
  #
  # # Remove large dataframes
  # rm(RR_OASI, X)
  #
  # # Dissimilarity matrix
  # dist.BH <- dist(X.scale, method = "euclidean")
  #



  mod_return()
}
