#' @title Estimation of the clusters according to the Kamila method.
#'
#' @description Estimation of the clusters according to the Kamila method.
#'
#' @param PARAM_GLOBAL dataframe with all needed parameters.
#' @param CATEG_DF subset of the register of rents containing all categorical
#' variables as factors.
#' @param CONT_DF subset of the register of rents containing all the continuous
#' variables
#' @return PLOTDATKAM database containing the clusters factor and the other
#' variables.
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
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")
  CATFACTOR <- as.data.frame(CATEG_DF)
  # summary(CONTVARS)



  # ## Construction of the clusters with the Kamila method
  # set.seed(5)
  #
  # numberofclusters <- 4
  #
  # kmres <- kamila(
  #   conVar = CONTVARS,
  #   catFactor = CATFACTOR,
  #   numClust = numberofclusters,
  #   numInit = 10,
  #   maxIter = 50
  # )
  #
  # # Transform the number of clusters into factors
  # fcluster <- factor(kmres$finalMemb)
  #
  # # Construction of a Dataframe for plotting the estimation results
  # PLOTDATKAM <- cbind(CONTVARS,
  #                     CONT_DF,
  #                     CATFACTOR,
  #                     fcluster)
  # path_rdata <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/rdata"
  #
  # save(PLOTDATKAM,
  #      file = file.path(path_rdata, paste0("PLOTDATKAM_",
  #                                     numberofclusters,
  #                                     "CLUSTERS")))

  set.seed(6)
  numberofclusters <- 2:3
  kmresps <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = numberofclusters,
    numInit = 10,
    maxIter = 5,
    calcNumClust = "ps"
  )
  browser()
  # Transform the number of clusters into factors
  fcluster <- factor(kmresps$finalMemb)

  PLOTDATKAMPS <- cbind(CONTVARS,
                            CONT_DF,
                            CATFACTOR,
                            fcluster)
  path_rdata <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/rdata"

  save(PLOTDATKAMPS,
       file = file.path(path_rdata, paste0("PLOTDATKAM_",
                                           numberofclusters,
                                           "CLUSTERS")))


  mod_return(PLOTDATKAM)
}
