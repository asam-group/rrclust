#' @title Read the data from the path in PARAM_GLOBAL
#' @description Read the data from the path in PARAM_GLOBAL
#'
#' @param path directory path
#' @return path_data
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
#'
path_data <- function(path) {
  # read from PARAM_GLOBAL
  tl_PARAM_GLOBAL <- param_tidylist_read(path)
  path_data <- tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]]
  path_data
}

#' @title Separate a string of several words at commas to a vector
#' @param x string of words to separate
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
separate_at_comma <- function(x) {
  if (!is.na(x)) {
    trimws(strsplit(x, ",")[[1]])
  }
}


#' @title Write parameters
#' @param x parameters tibble to write
#' @param file file to write to
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @autoglobal
#' @export
write_param <- function(x, file) {
  x |>
    gather(key, value) |>
    data.table::fwrite(file = file, sep = ";")
}


#' @title Read a single parameters csv file
#' @param file file
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export

read_param <- function(file) {
  z0 <- data.table::fread(file = file, sep = ";") |>
    as_tibble()

  z1 <- spread(z0, key, value, convert = TRUE)

  if (identical(dim(z1), c(0L, 0L))) {
    return(z1)
  }

  select(z1, one_of(z0[["key"]]))
}


#' @title Read all files from a folder and return a tidylist
#' @param path path
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @return tidylist ll
#' @export
param_tidylist_read <- function(path) {
  param.files <- list.files(path, pattern = "^PARAM_.*csv$")
  ll <- lapply(file.path(path, param.files), read_param)
  names(ll) <- gsub("\\.csv", "", param.files)
  tidylist_ensure(ll)
}


#' @title Function to create a single identifier number
#' @param method_name name of the clustering method
#' @param path directory path
#' @return identifier_number container identifier
#' @export
clustmeth_identifier_number <- function(method_name,
                                        path) {
  params_folder_suffix <- gsub("^.+?_", "", basename(path))

  identifier_number <- paste0(
    "cl_",
    method_name,
    "_",
    format(Sys.time(), format = "%Y%m%d%H%M%S"),
    "_",
    tolower(Sys.getenv("USERNAME")),
    "_",
    params_folder_suffix
  )
  identifier_number
}

#' @title Function to create a the path of the parameters
#' @param path directory path
#' @export
path_param <- function(path) {
  # legacy mode
  if (check_container_dir(path)) {
    file.path(path, "param")
  } else {
    path
  }
}

#' @ title Check if a directory looks like a container
#' @param from directory to container
#' @export
check_container_dir <- function(from) {
  from <- normalizePath(from, mustWork = TRUE)

  root <- list.files(from)

  # output containers contain an input container
  if ("inp_container" %in% root) root <- list.files(file.path(from, "inp_container"))

  required <- c("param", "inp")

  missing <- setdiff(required, root)

  if (length(missing) > 0) {
    # message("missing in ", from, ": ", paste(missing, collapse = ", "))
    return(FALSE)
  }
 
  TRUE
}
