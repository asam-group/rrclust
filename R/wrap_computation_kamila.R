#' Wrapper for the clusters construction.
#'
#' This wrapper contains all the necessary modules which allow to
#' construct the clusters.
#'
#' @param tl_inp List of input data frames of which we use:
#' - `PARAM_KAMILA$calc_kstar`: If TRUE, estimates the clusters. Else, takes the
#' parameter PARAM_KAMILA$param_kstar.
#' - `PARAM_KAMILA$cont_var_expl`: List of continuous variables chosen as
#' explicative variables.
#' - `PARAM_KAMILA$categ_var_expl`: List of categorical variables chosen as
#' explicative variables.
#'
#' @param tl_prepadata List of data frames prepared in a first step.
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_mod_calc_kamila`: Tidylist containing
#'      * `PLOTDATKAM` data frame containing the clusters factor and the other
#'      variables.
#'      * `KM_RES_FINAL` data frame containing the resulting parameters of the
#'      clustering.
#'      * `CONTVARS` data frame containing the continuous standardised
#'      variables.
#'      * `FULL_CONT_DF` data frame containing the continuous variables used
#'      for the estimation.
#'      * `FULL_CATEG_DF` data frame containing the categorical variables used
#'      for the estimation.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#'
#' @export

wrap_computation_kamila_ <- function(tl_inp,
                                     tl_prepadata) {
  cont_var_expl <- separate_at_comma(tl_inp$PARAM_KAMILA$cont_var_expl)

  CONT_DF_TS <- tl_prepadata$CONT_DF_TS |>
    select(any_of(cont_var_expl))

  CONT_DF <- tl_prepadata$CONT_DF |>
    select(any_of(cont_var_expl))

  categ_var_expl <- separate_at_comma(tl_inp$PARAM_KAMILA$categ_var_expl)

  CATEG_DF_TS <- tl_prepadata$CATEG_DF_TS |>
    select(any_of(categ_var_expl))

  CATEG_DF <- tl_prepadata$CATEG_DF |>
    select(any_of(categ_var_expl))

  # Run the algorithm on the TS to find the optimal number of clusters kstar
  # Writes kstar as the parameter PARAM_KAMILA$param_kstar
  if (tl_inp$PARAM_KAMILA$calc_kstar) {
    tl_mod_kstar <- mod_kstar(
      PARAM_KAMILA = tl_inp$PARAM_KAMILA,
      CATEG_DF_TS = CATEG_DF_TS,
      CONT_DF_TS = CONT_DF_TS
    )
    KM_RES <- tl_mod_kstar$KM_RES
  } else {
    KM_RES <- tibble(cluster_id = NA_real_) |>
      mutate(
        kstar = NA_real_,
        ps_values = NA_real_, # Prediction Strength value
        avg_pred_str = NA_real_, # Average prediction strength
        std_err_pred_str = NA_real_, # SE pred. strength
        ps_cv_res_run = NA_real_, # Pred. Strength CV residuals
        cluster_id = NA_real_
      )
  }

  # PARAM_KAMILA with the updated kstar parameter
  PARAM_KAMILA <- if (tl_inp$PARAM_KAMILA$calc_kstar) {
    tl_mod_kstar$PARAM_KAMILA
  } else {
    tl_inp$PARAM_KAMILA
  }


  # Apply kstar to the whole dataset
  tl_mod_calc_kamila <- mod_calc_kamila(
    PARAM_KAMILA = PARAM_KAMILA,
    CONT_DF = CONT_DF,
    FULL_CONT_DF = tl_prepadata$CONT_DF,
    CATEG_DF = CATEG_DF,
    FULL_CATEG_DF = tl_prepadata$CATEG_DF,
    KM_RES = KM_RES
  )

  if (tl_inp$PARAM_KAMILA$calc_kstar) {
    c(
      tl_mod_calc_kamila,
      tl_mod_kstar
    )
  } else {
    c(tl_mod_calc_kamila)
  }
}


#' Memoised copy of \code{\link{wrap_computation_kamila}}
#' Create a memoised copy of \code{\link{wrap_computation_kamila}}
#' @param tl_inp List of input data frames of which we use:
#' - `PARAM_KAMILA$calc_kstar`: If TRUE, estimates the clusters. Else, takes the
#' parameter PARAM_KAMILA$param_kstar.
#' - `PARAM_KAMILA$cont_var_expl`: List of continuous variables chosen as
#' explicative variables.
#' - `PARAM_KAMILA$categ_var_expl`: List of categorical variables chosen as
#' explicative variables.
#'
#' @param tl_prepadata List of data frames prepared in a first step.
#' @return a `tidylist` containing the following tidylists:
#' - `tl_mod_calc_kamila`
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_computation_kamila <- memoise::memoise(wrap_computation_kamila_)
