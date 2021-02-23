#' @title Estimation of the clusters according to the K-means method.
#'
#' @description Estimation of the clusters according to the K-means method.
#'
#' @param tl_inp_kamila set of the needed output.
#' @return
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-02-23 / Llc

mod_calc_kamila <- function(PARAM_GLOBAL,
                            CATEG_DF,
                            CONT_DF,
                            list = NULL) {
  mod_init()

  # Standardize the continuous variables
  CONTVARS <- as.data.frame(lapply(CONT_DF, rangeStandardize))
  # summary(CONTVARS)

  browser()

  # Construction of the clusters with the Kamila method
  set.seed(5)
  kmres <- kamila(
    conVar = CONTVARS,
    catFactor = CATEG_DF,
    numClust = 5,
    numInit = 10,
    maxIter = 50
  )

  # Transform the number of clusters into factors
  fcluster <- factor(kmres$finalMemb)

  # Construction of a Dataframe for plotting the estimation results
  PLOTDATKAM <- cbind(CONTVARS, CATEG_DF, fcluster)

  browser()

  # set.seed(6)
  # numberOfClusters <- 2:10
  # kmresPs <- kamila(
  #   conVar = CONTVARS,
  #   catFactor = CATEG_DF,
  #   numClust = numberOfClusters,
  #   numInit = 10,
  #   maxIter = 50,
  #   calcNumClust = "ps"
  # )

  mod_return(plotDatKam)
}
