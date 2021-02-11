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
                   numClust = 6,
                   numInit = 10)

 #  psPlot <- with(kamRes$nClust, qplot(numberOfClusters, psValues) +
 #                   + geom_errorbar(aes(x = numberOfClusters, ymin = psValues - stdErrPredStr,
 #                                       + ymax = psValues + stdErrPredStr), width = 0.25))
 # psPlot <- psPlot + geom_hline(yintercept = 0.8, lty = 2)
 # psPlot + scale_x_continuous(breaks = numberOfClusters) + ylim(0, 1.1)
 # table(kamila3 = kmRes$finalMemb, kamila2 = kamRes$finalMemb)


  mod_return()
}
