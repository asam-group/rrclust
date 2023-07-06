#' Function to read the data, to execute the computations and write the
#' data and the parameters (of the inputs and the outputs).
#'
#' This function executes the computations according to the
#' parameters which are read in a first step. In a third step, the outputs are
#' delivered.
#'
#' @param path paths of the set of parameters set to a particular output. More
#' than 1 path can be set.
#'
#' @param ... any other arguments
#'
#' @param fun function to apply to those sets of parameters.
#'
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
multi_run <- function(path, fun, ...) {
  verbose_fun <- function(path) {
    message("-----------------------------------------------------------------")
    message("Processing: ", path)
    message("-----------------------------------------------------------------")
    fun(path, ...)
  }

  ans <- lapply(path, verbose_fun)
  unlist(ans)
}
