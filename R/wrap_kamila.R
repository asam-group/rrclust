#' @title wrap_kamila
#' @description wrap_kamila is a simple function which executes the computations
#' following th kamila clustering method.
#' @param tl_inp tidylist of inputs
#' @return tidylists
#' @export
#'
# Last change: 2021-02-28 / Llc

wrap_kamila_ <- function(tl_inp_kamila) {

  # Dataset preparation
  tl_prepadata <- wrap_prepadata(tl_inp = tl_inp_kamila)

  # Main computation
  tl_computation_kamila <- wrap_computation_kamila(
    tl_inp = tl_inp_kamila,
    tl_prepadata = tl_prepadata
  )

  # Output
  tl_computation_kamila
}


#' @title wrap_kamila (memoised)
#' @export
wrap_kamila <- memoise::memoise(wrap_kamila_)
