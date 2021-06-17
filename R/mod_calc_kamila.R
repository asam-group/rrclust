#' @title Splitting the initial dataset into kstar clusters.
#'
#' @description Splitting the initial dataset into kstar clusters by using the
#' parameter kstar determined in the module \code{\link{mod_kstar}}.
#'
#' @param PARAM_KAMILA dataframe with all needed parameters for the Kamila method,
#' from which the following parameters are used:
#' - `numinit`: The number of initializations used.
#' - `maxiter`: The maximum number of iterations in each run.
#' - `param_kstar`: Best number of clusters estimated in the module
#' \code{\link{mod_kstar}}.
#'
#' @param CATEG_DF subset of the register of rents containing all categorical
#' variables as factors except for the nominal variables marital_stat and benef_type.
#'
#' @param CONT_DF subset of the register of rents containing all the continuous
#' variables except for the outcome variables aadr and monthly_rent.
#'
#' @param FULL_CONT_DF database containing the continuous variables used for the
#' estimation plus the outcome variables aadr and monthly_rent.
#'
#' @param FULL_CATEG_DF database containing the categorical variables used for
#' the estimation plus the nominal variables marital_stat and benef_type.
#'
#' @return a `tidylist` containing the following tidy data frames:
#' - `PLOTDATKAM` database containing the clusters factor and the other
#' variables.
#' - `KM_RES_FINAL` database containing the resulting parameters of the
#' clustering.
#' - `CONTVARS` database containing the continuous standardised variables.
#' - `FULL_CONT_DF` database containing the continuous variables used for the
#' estimation.
#' - `FULL_CATEG_DF` database containing the categorical variables used for
#' the estimation.
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-06-02 / Llc

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

  kstar <- PARAM_KAMILA$param_kstar

  kmres <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = kstar,
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
    # CATFACTOR,
    FULL_CATEG_DF
  ) %>%
    as_tibble()

  mod_return(
    PLOTDATKAM,
    CONTVARS,
    FULL_CONT_DF,
    FULL_CATEG_DF,
    KM_RES_FINAL
  )
}
