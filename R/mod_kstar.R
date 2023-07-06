#' Estimation of the best number of clusters using the Kamila algorithm.
#'
#' @description Estimation of best number of clusters using the Kamila algorithm
#' on the training set.
#'
#' @param PARAM_KAMILA data frame with all needed parameters for the Kamila
#' method, from which the following parameters are used:
#' - `numberofclusters`: The number of clusters returned by the algorithm, i.e.
#' sequence indicating the number of clusters which should be investigated to
#' extract the optimal number of clusters.
#' - `numinit`: The number of initializations used.
#' - `maxiter`: The maximum number of iterations in each run.
#' - `calcnumclust`: Character: Method for selecting the number of clusters.
#' Setting calcNumClust to ’ps’ uses the prediction strength method of
#' Tibshirani &Walther (J. of Comp. and Graphical Stats. 14(3), 2005).
#' - `pred_threshold`: Threshold fixed to 0.8 for well separated clusters (i.e.
#' not overlapping).
#'
#' @param CATEG_DF_TS Training set of the pension register containing all
#' categorical variables as factors.
#'
#' @param CONT_DF_TS Training set of the pension register containing all the
#' continuous variables.
#'
#' @param list List of input data frames.
#'
#' @return a tidylist containing the following tidy data frames:
#'  - `KM_RES` Data frame containing the results of the clustering.
#'  - `PARAM_KAMILA` Data frame with the updated kstar parameter.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#'
#' @export
#' @autoglobal
mod_kstar <- function(PARAM_KAMILA,
                      CATEG_DF_TS,
                      CONT_DF_TS,
                      list = NULL) {
  mod_init()

  CONTVARS <- as.data.frame(lapply(CONT_DF_TS, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF_TS)

  set.seed(6)

  # Number of clusters to be returned by the algorithm
  numberofclusters <- as.numeric(eval(parse(
    text =
      PARAM_KAMILA$numberofclusters
  )))

  kmresps <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = numberofclusters,
    numInit = PARAM_KAMILA$numinit,
    maxIter = PARAM_KAMILA$maxiter,
    calcNumClust = PARAM_KAMILA$calcnumclust,
    predStrThresh = PARAM_KAMILA$pred_threshold
  )

  KSTAR <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) |>
    mutate(kstar = kmresps$nClust$bestNClust)

  NCLUST <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) |>
    mutate(
      # Prediction Strength value, PS = 1 - Variance
      ps_values = kmresps$nClust$psValues,
      # Average prediction strength
      avg_pred_str = kmresps$nClust$avgPredStr,
      # SE pred. strength
      std_err_pred_str = kmresps$nClust$stdErrPredStr
    )


  PS_CV_RES <- kmresps$nClust$psCvRes |> # Pred. Strength CV residuals
    as_tibble() |>
    mutate(cluster_id = as.integer(rownames(kmresps$nClust$psCvRes)))
  colnames(PS_CV_RES)[!grepl(
    "cluster_id",
    colnames(PS_CV_RES)
  )] <- paste("ps_cv_res_run",
    1:PARAM_KAMILA$numinit,
    sep = "_"
  )

  KM_RES <- KSTAR |>
    left_join(NCLUST,
      by = "cluster_id"
    ) |>
    left_join(PS_CV_RES,
      by = "cluster_id"
    )

  # Save the optimal number of clusters in a parameter
  PARAM_KAMILA$param_kstar <- kmresps$nClust$bestNClust


  mod_return(
    KM_RES,
    PARAM_KAMILA
  )
}
