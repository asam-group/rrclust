#' Write a message and execute the function applied on one or more containers
#'
#' Write a message and execute the function applied on one or more containers.
#'
#' @param path Path of the parameters container.
#' @param fun Function to run for each of the given paths.
#' @param ... Any other arguments.
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
