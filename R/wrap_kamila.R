#' @title Wrapper to execute the Kamila algorithm.
#'
#' @description Simple function which executes the computations needed for the
#' Kamila algorithm.
#'
#' @param tl_inp_kamila tidylist of inputs
#'
#' @return a `tidylist` containing the following tidylists:
#' - `tl_computation_kamila`
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#'
#' @export
wrap_kamila_ <- function(tl_inp_kamila) {
  tl_prepadata <- wrap_prepadata(tl_inp = tl_inp_kamila)
  tl_computation_kamila <- wrap_computation_kamila(
    tl_inp = tl_inp_kamila,
    tl_prepadata = tl_prepadata
  )
  tl_computation_kamila
}


#' @title Memoised copy of \code{\link{wrap_kamila}}
#' @description Create a memoised copy of \code{\link{wrap_kamila}}
#' @param tl_inp_kamila tidylist of inputs
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
wrap_kamila <- memoise::memoise(wrap_kamila_)
