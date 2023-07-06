#' Write and replace parameters
#'
#' Write and replace parameters in the tibble `PARAM_ALL`.
#'
#' @param tl Tidylist.
#' @param ... Any other arguments.
#' @param param_replace Parameter which can be used to test some other
#'   parameters.
#' @param update_param_all If TRUE, update all parameters.
#'
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @export
param_replace <- function(tl,
                          ...,
                          param_replace = NULL,
                          update_param_all = TRUE) {
  # non-list input via ...
  ll <- list(...)
  if (is.null(param_replace)) {
    if (length(ll) > 0) param_replace <- ll
  }

  replace_one_value_in_tl <- function(name, value, tl) {
    spl <- strsplit(name, split = ".", fixed = TRUE)[[1]]
    tbl.name <- spl[1]
    # do nothing if not there
    if (!(tbl.name %in% names(tl))) {
      return(tl)
    }
    var.name <- spl[2]
    tl[[tbl.name]][[var.name]] <- value
    tl
  }

  ans <- tl
  if (!is.null(param_replace)) {
    for (i in seq_along(param_replace)) {
      ans <- replace_one_value_in_tl(
        name = names(param_replace[i]),
        value = unname(param_replace[[i]]),
        ans
      )
    }
  }

  if (update_param_all) ans <- param_all_update(ans)

  ans
}


#' Update the PARAM_ALL
#'
#' Update the PARAM_ALL.
#'
#' @param tl Tidylist of parameter sets.
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @export
param_all_update <- function(tl) {
  param_tbls <- grep("^PARAM", names(tl), value = TRUE)
  param_tbls_no_all <- tl[setdiff(param_tbls, "PARAM_ALL")]

  # only keep dfs with NCOL > 0 (non-empty dfs)
  param_tbls_non_empty <- Filter(function(e) NCOL(e) > 0, param_tbls_no_all)

  param_list <- unlist(param_tbls_non_empty, recursive = FALSE)
  tl$PARAM_ALL <- as_tibble(param_list)
  tl
}
