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
                            CATEG_DF_TS,
                            CONT_DF_TS,
                            # CATEG_DF_VS,
                            # CONT_DF_VS,
                            # CONT_DF,
                            # CATEG_DF,
                            list = NULL) {
  mod_init()


  path_rdata <- PARAM_GLOBAL$path_rdata

  # Standardize the continuous variables

  CONTVARS <- as.data.frame(lapply(CONT_DF_TS, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF_TS)


  # summary(CONTVARS)
  # rm(CONT_DF_TS)
  # rm(CATEG_DF_TS)
  ## Construction of the clusters with the Kamila method
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
  #
  # save(PLOTDATKAM,
  #      file = file.path(path_rdata, paste0("PLOTDATKAM_",
  #                                     numberofclusters,
  #                                     "CLUSTERS")))

  #--- with calcNumClust = "ps"---------------------------------------------------
  set.seed(6)
  numberofclusters <- 2:10
  kmresps <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = numberofclusters,
    numInit = 10,
    maxIter = 50,
    calcNumClust = "ps"
  )


  # Transform the number of clusters into factors
  fclusterps <- factor(kmresps$finalMemb)

  PLOTDATKAMPS <- cbind(
    CONTVARS,
    CONT_DF_TS,
    CATFACTOR,
    fclusterps
  )

  # Save the data
  save(PLOTDATKAMPS,
    file = file.path(path_rdata, paste0(
      "PLOTDATKAMPS_",
      "CLUSTERS.RData"
    ))
  )
  save(kmresps,
    file = file.path(path_rdata, "kmresps.RData")
  )

  mod_return(
    # PLOTDATKAM,
    PLOTDATKAMPS
    # kmres,
    # kmresps
  )
}
