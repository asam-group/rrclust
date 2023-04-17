#' @title Preparation of the kamila results of the training set.
#'
#' @description Prepares the kamila results of the training set and saves the
#' optimal number of clusters kstar.
#'
#' @param PARAM_INPUTS a data frame containing the data paths.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `KM_RES` : contains the kamila results of the training set.
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @autoglobal
#' @export

# - `Last change`: 2021-03-03 / Llc

mod_input_kmres <- function(PARAM_INPUTS,
                            list = NULL) {
  mod_init()

  load(file = file.path(
    PARAM_INPUTS$path_kmres,
    PARAM_INPUTS$file_kmres
  ))

  # Optimal number of clusters
  KSTAR <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) |>
    mutate(kstar = kmresps$nClust$bestNClust)

  # Other information of the run
  NCLUST <- tibble(cluster_id = as.integer(names(kmresps$nClust$psValues))) |>
    mutate(
      psvalues = kmresps$nClust$psValues,
      avgpredstr = kmresps$nClust$avgPredStr,
      stderrpredstr = kmresps$nClust$stdErrPredStr
    )

  fun_rename <- function(x) gsub("Run ", "run_", x)

  PS_CV_RES <- kmresps$nClust$psCvRes |>
    as_tibble() |>
    mutate(cluster_id = as.integer(rownames(kmresps$nClust$psCvRes))) |>
    rename_if(!grepl("^Run", .), fun_rename) |>
    mutate_all(as.numeric)


  KM_RES <- KSTAR |>
    left_join(NCLUST,
      by = "cluster_id"
    ) |>
    left_join(PS_CV_RES,
      by = "cluster_id"
    )

  mod_return(
    KM_RES
  )
}
