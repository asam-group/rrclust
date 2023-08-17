#' @examples
#' test_message <- function(a) {
#'   message("this is test from function message.")
#'   return(a)
#' }
#'
#' test_warning <- function(a) {
#'   warning("this is test from function warning.")
#'   return(a)
#' }
#'
#' test_error <- function(a) {
#'   stop("this is test from function error.")
#'   return(a)
#' }
#'
#' capture_log1(test_message)(1)
#'
#' capture_log1(test_warning)(1)
#'
#' capture_log1(test_error)(1)

