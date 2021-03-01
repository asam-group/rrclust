#' @title Estimation of the clusters according to the Kamila method.
#'
#' @description Estimation of the clusters according to the Kamila method.
#'
#' @param PARAM_GLOBAL dataframe with all needed parameters.
#' @param CATEG_DF subset of the register of rents containing all categorical
#' variables as factors.
#' @param CONT_DF subset of the register of rents containing all the continuous
#' variables
#' @param CATEG_DF_TS Training set of the register of rents containing all categorical
#' variables as factors.
#' @param CONT_DF_TS Training set of the register of rents containing all the continuous
#' variables
#' @return PLOTDATKAM database containing the clusters factor and the other
#' variables.
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-02-26 / Llc

mod_calc_kamila <- function(PARAM_GLOBAL,
                            CATEG_DF_TS,
                            CONT_DF_TS,
                            # CATEG_DF_VS,
                            # CONT_DF_VS,
                            CONT_DF,
                            CATEG_DF,
                            list = NULL) {
  mod_init()


  path_rdata <- PARAM_GLOBAL$path_rdata

  #--- 1.1) Standardize the continuous variables ---------------------------------

  CONTVARS <- as.data.frame(lapply(CONT_DF_TS, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF_TS)

  #--- 1.2) Estimate the best number of clusters g* ------------------------------
  # Needs to be done only once. Saved under an .RData file.

  # Setting seed to generate a reproducible random sampling
  # set.seed(6)
  # numberofclusters <- 2:20
  # kmresps <- kamila(
  #   conVar = CONTVARS,
  #   catFactor = CATFACTOR,
  #   numClust = numberofclusters,
  #   numInit = 10,
  #   maxIter = 50,
  #   calcNumClust = "ps"
  # )
  #
  #
  # # Transform the number of clusters into factors
  # fclusterps <- factor(kmresps$finalMemb)
  # browser()
  # PLOTDATKAMPS <- cbind(
  #   CONTVARS,
  #   CONT_DF_TS,
  #   CATFACTOR,
  #   fclusterps
  # ) %>%
  #   as_tibble()
  #
  # # Save the data
  # save(PLOTDATKAMPS,
  #      file = file.path(path_rdata, paste0(
  #        "PLOTDATKAMPS_",
  #        "CLUSTERS.RData"
  #      ))
  # )
  # save(kmresps,
  #      file = file.path(path_rdata, "kmresps.RData")
  # )
  #
  #--- 2.1 Construction of the g* (from 1.2) clusters with the Kamila method----

  CONTVARS <- as.data.frame(lapply(CONT_DF, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF)

  #--- 2.2 Construction of the g* (from 1.2) clusters with the Kamila method on
  # the whole dataset ----------------------------------------------------------


  load(file = file.path(path_rdata, "kmresps.RData"))

  # Setting seed to generate a reproducible random sampling
  set.seed(5)

  gstar <- kmresps$nClust$bestNClust

  kmres <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = gstar,
    numInit = 10,
    maxIter = 50
  )

  # Transform the number of clusters into factors
  fcluster <- factor(kmres$finalMemb)

  # Construction of a Dataframe for plotting the estimation results
  PLOTDATKAM <- cbind(
    CONTVARS,
    CONT_DF,
    CATFACTOR,
    fcluster
  ) %>%
    as_tibble()

  save(PLOTDATKAM,
    file = file.path(path_rdata, paste0(
      "PLOTDATKAM_",
      gstar,
      "CLUSTERS.RData"
    ))
  )

  mod_return(
    PLOTDATKAM
  )
}
