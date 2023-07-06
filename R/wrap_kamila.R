#' Run Kamila algorithm
#'
#' Run Kamila algorithm.
#'
#' @param tl_inp_kamila Tidylist of inputs.
#'
#' @return `tl_computation_kamila` with the following data frames:
#'   - `PLOTDATKAM` Data frame containing the clusters factor and the other
#'   variables.
#'   - `KM_RES_FINAL` Data frame containing the resulting parameters of the
#'   clustering.
#'   - `CONTVARS` Data frame containing the continuous standardised variables.
#'   - `FULL_CONT_DF` Data frame containing the continuous variables used for
#'   the estimation.
#'   - `FULL_CATEG_DF` Data frame containing the categorical variables used for
#'   the estimation.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_kamila_ <- function(tl_inp_kamila) {
  tl_prepadata <- wrap_prepadata(tl_inp = tl_inp_kamila)
  tl_computation_kamila <- wrap_computation_kamila(
    tl_inp = tl_inp_kamila,
    tl_prepadata = tl_prepadata
  )
  tl_computation_kamila
}


#' Memoise the function \code{\link{wrap_kamila}}
#'
#' Create a memoised copy of \code{\link{wrap_kamila}}.
#'
#' @param tl_inp_kamila Tidylist of inputs.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_kamila <- memoise::memoise(wrap_kamila_)
