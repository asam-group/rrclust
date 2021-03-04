#' @title Estimation of the clusters according to the Kamila method.
#'
#' @description Estimation of the clusters according to the Kamila method on the
#' training set.
#'
#' @param PARAM_KAMILA dataframe with all needed parameters for the Kamila method,
#' from which the following parameters are used:
#' - `calc_gstar`: If TRUE, estimates the clusters.
#' - `numberofclusters`: The number of clusters returned by the algorithm, i.e.
#' sequence indicating the number of clusters which should be investigated to
#' extract the optimal number of clusters.
#' - `numinit`: The number of initializations used.
#' - `maxiter`: The maximum number of iterations in each run.
#' - `calcnumclust`: Character: Method for selecting the number of clusters. Setting
#' calcNumClust to ’ps’ uses the prediction strength method of
#' Tibshirani &Walther (J. of Comp. and Graphical Stats. 14(3), 2005).
#' @param CATEG_DF_TS Training set of the register of rents containing all categorical
#' variables as factors.
#' @param CONT_DF_TS Training set of the register of rents containing all the continuous
#' variables
#' @return KM_RES database containing the results of the clustering.
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @export
#' @import kamila

# Last change: 2021-03-04 / Llc

mod_gstar <- function(PARAM_KAMILA,
                      CATEG_DF_TS,
                      CONT_DF_TS,
                      list = NULL) {
  mod_init()


  #--- 1.1) Standardize the continuous variables -------------------------------

  CONTVARS <- as.data.frame(lapply(CONT_DF_TS, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF_TS)

  #--- 1.2) Estimate the best number of clusters g* ----------------------------

  # Computes the clusters and reruns the inputs with the newest results.

  # Setting seed to generate a reproducible random sampling
  set.seed(6)

  # Number of clusters to be returned by the algorithm
  numberofclusters <- as.numeric(eval(parse(
    text =
      PARAM_KAMILA$numberofclusters
  )))

  # Running the algorithm on the Training Set
  kmresps <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = numberofclusters,
    numInit = PARAM_KAMILA$numinit,
    maxIter = PARAM_KAMILA$maxiter,
    calcNumClust = PARAM_KAMILA$calcnumclust,
    predStrThresh = PARAM_KAMILA$pred_threshold
  )


  # Optimal number of clusters
  GSTAR <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) %>%
    mutate(gstar = kmresps$nClust$bestNClust)

  # Other information of the run
  NCLUST <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) %>%
    mutate(
      ps_values = kmresps$nClust$psValues, # Prediction Strength value
      avg_pred_str = kmresps$nClust$avgPredStr, # Average prediction strength
      std_err_pred_str = kmresps$nClust$stdErrPredStr # SE pred. strength
    )


  PS_CV_RES <- kmresps$nClust$psCvRes %>% # Pred. Strength CV residuals
    as_tibble() %>%
    mutate(cluster_id = as.integer(rownames(kmresps$nClust$psCvRes)))
  colnames(PS_CV_RES)[!grepl(
    "cluster_id",
    colnames(PS_CV_RES)
  )] <- paste("ps_cv_res_run",
    1:PARAM_KAMILA$numinit,
    sep = "_"
  )

  # Join all datasets of results
  KM_RES <- GSTAR %>%
    left_join(NCLUST,
      by = "cluster_id"
    ) %>%
    left_join(PS_CV_RES,
      by = "cluster_id"
    )

  # Save the optimal number of clusters in a parameter
  PARAM_KAMILA$param_gstar <- kmresps$nClust$bestNClust


  mod_return(KM_RES)
}
