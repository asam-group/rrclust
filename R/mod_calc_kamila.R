#' Splitting the initial dataset into kstar clusters.
#'
#' Splitting the initial dataset into kstar clusters by using the
#' parameter kstar determined in the module \code{\link{mod_kstar}}.
#'
#' @param PARAM_KAMILA data frame with all needed parameters for the Kamila
#' method, from which the following parameters are used:
#' - `numinit`: The number of initializations used.
#' - `maxiter`: The maximum number of iterations in each run.
#' - `param_kstar`: Best number of clusters estimated in the module
#' \code{\link{mod_kstar}}.
#'
#' @param CATEG_DF Subset of the pension register containing all categorical
#' variables as factors except for the nominal variables marital_stat and
#' benef_type.
#'
#' @param CONT_DF Subset of the pension register containing all the continuous
#' variables except for the outcome variables aadr and monthly_pension.
#'
#' @param FULL_CONT_DF Data frame containing the continuous variables used for
#' the estimation plus the outcome variables aadr and monthly_pension.
#'
#' @param FULL_CATEG_DF Data frame containing the categorical variables used for
#' the estimation plus the nominal variables marital_stat and benef_type.
#'
#' @param KM_RES Tibble contains the kamila results of the training set.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#' - `PLOTDATKAM` Data frame containing the clusters factor and the other
#' variables.
#' - `KM_RES_FINAL` Data frame containing the resulting parameters of the
#' clustering.
#' - `CONTVARS` Data frame containing the continuous standardised variables.
#' - `FULL_CONT_DF` Data frame containing the continuous variables used for the
#' estimation.
#' - `FULL_CATEG_DF` Data frame containing the categorical variables used for
#' the estimation.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
mod_calc_kamila <- function(PARAM_KAMILA,
                            CONT_DF,
                            CATEG_DF,
                            FULL_CONT_DF,
                            FULL_CATEG_DF,
                            KM_RES,
                            list = NULL) {
  mod_init()

  CONTVARS <- as.data.frame(lapply(CONT_DF, rangeStandardize))
  names(CONTVARS) <- paste0(names(CONTVARS), "_std")

  CATFACTOR <- as.data.frame(CATEG_DF)

  set.seed(5)

  kstar <- PARAM_KAMILA$param_kstar

  kmres <- kamila(
    conVar = CONTVARS,
    catFactor = CATFACTOR,
    numClust = kstar,
    numInit = PARAM_KAMILA$numinit,
    maxIter = PARAM_KAMILA$maxiter
  )

  cluster_id <- factor(kmres$finalMemb)

  KM_RES_FINAL <- tibble(
    final_loglik = kmres$finalLogLik,
    final_obj = kmres$finalObj,
    num_clust = kmres$input$numClust,
    max_iterations = kmres$input$maxIter,
    categorical_bw = kmres$input$catBw
  )

  PLOTDATKAM <- cbind(
    cluster_id,
    CONTVARS,
    FULL_CONT_DF,
    FULL_CATEG_DF
  ) |>
    as_tibble()

  mod_return(
    PLOTDATKAM,
    CONTVARS,
    FULL_CONT_DF,
    FULL_CATEG_DF,
    KM_RES_FINAL
  )
}
