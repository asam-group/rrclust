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
                            CATEG_DF,
                            CONT_DF,
                            list = NULL) {
  mod_init()
browser()


  kamRes <- kamila(conVar = CONT_DF,
                   catFactor = CATEG_DF,
                   numClust = 2,
                   numInit = 10)

  # table(kamRes$finalMemb, dat$trueID)


  mod_return()
}
