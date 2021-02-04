#' @title wrap_kmeans
#' @description wrap_kmeans is a simple function which executes the computations
#' following th kmeans clustering method.
#' @param tl_inp tidylist of inputs
#' @return tidylists
#' @export
#'

wrap_kmeans_ <- function(tl_inp_kmeans){

  # tl_preliminary_calc
  # tl_princ_computation

  tidylist(NULL)
}


#' title wrap_ahv (memoised)
#' @export
wrap_kmeans <- memoise::memoise(wrap_kmeans_)
