#' @title Estimation of the clusters according to the Kamila method.
#'
#' @description Estimation of the clusters according to the Kamila method.
#'
#' @param PARAM_KAMILA dataframe with all needed parameters for the Kamila method,
#' from which the following parameters are used:
#' - `numinit`: The number of initializations used.
#' - `maxiter`: The maximum number of iterations in each run.
#' @param CATEG_DF subset of the register of rents containing all categorical
#' variables as factors.
#' @param CONT_DF subset of the register of rents containing all the continuous
#' variables
#' @return PLOTDATKAM database containing the clusters factor and the other
#' variables.
#' @return KM_RES_FINAL database containing the resulting parameters of the
#' clustering.
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-03-04 / Llc

mod_calc_kamila <- function(PARAM_KAMILA,
                            CONT_DF,
                            CATEG_DF,
                            FULL_CONT_DF,
                            FULL_CATEG_DF,
                            list = NULL) {
  mod_init()


  #--- 1.1 Construction of the g* (from 1.2) clusters with the Kamila method----

  CONTVARS <- as.data.frame(lapply(CONT_DF, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF)

  #--- 1.2 Construction of the g* (from 1.2) clusters with the Kamila method on
  # the whole dataset ----------------------------------------------------------

  # Setting seed to generate a reproducible random sampling
  set.seed(5)

  gstar <- PARAM_KAMILA$param_gstar

  kmres <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = gstar,
    numInit = PARAM_KAMILA$numinit,
    maxIter = PARAM_KAMILA$maxiter
  )

  # Transform the number of clusters into factors
  cluster_id <- factor(kmres$finalMemb)

  # Retrieve all clustering estimation resulting parameters
  KM_RES_FINAL <- tibble(
    final_loglik = kmres$finalLogLik,
    final_obj = kmres$finalObj,
    num_clust = kmres$input$numClust,
    max_iterations = kmres$input$maxIter,
    categorical_bw = kmres$input$catBw
  )

  # Construction of a Dataframe for plotting the estimation results
  PLOTDATKAM <- cbind(
    cluster_id,
    CONTVARS,
    FULL_CONT_DF,
    CATFACTOR
  ) %>%
    as_tibble()

  mod_return(
    PLOTDATKAM,
    KM_RES_FINAL
  )
}
