#' Convert SAS files to R files
#'
#' @param path directory path
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @param path directory path
#' @export
sas7bdat_to_rds <- function(path) {
  library(sas7bdat)

  # all files with ending .sas
  sas.files <- list.files(path, pattern = "\\.sas7bdat$", full.names = TRUE)

  for (sas.files.i in sas.files) {
    message("converting ", basename(sas.files.i), appendLF = FALSE)
    dta <- read.sas7bdat(sas.files.i)

    # ?see saveRDS
    saveRDS(dta, file = gsub("\\.sas7bdat$", ".rds", sas.files.i))
    message("converting ", basename(sas.files.i))
  }
}

#' @title Read the data from the path in PARAM_GLOBAL
#' @description Read the data from the path in PARAM_GLOBAL
#'
#' @param path directory path
#' @return path_data
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
#'
path_data <- function(path){
  #read from PARAM_GLOBAL
  tl_PARAM_GLOBAL <- param_tidylist_read(path)
  path_data <- tl_PARAM_GLOBAL$PARAM_GLOBAL[['path_data']]
}

#' Converting Data Frames to Matrices and Back
#'
#' @param x a tidy data frame with 3 columns. The values in the first column are
#'   mapped to the vertical axis of the resulting matrix, the values of the
#'   second colum to the columns of the matrix. The matix is filled with the
#'   values from the third column.
#'
#' @param template a tidy data frame with 3 columns. Servers as a template to re
#'   convert a matrix to a data frame. Usually the data frame that was used to
#'   create the matrix in the beginning.
#'
#' @param X a matrix.
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @examples
#' library(tidyr)
#' library(dplyr)
#'
#' # tibble must be of the form:
#' # 1. column: y axis
#' # 2. column: x axis
#' # 3. column: value
#'
#' POP <- crossing(age = 20:99, year = 2000:2020) %>%
#'   mutate(my_var = rnorm(nrow(.)))
#'
#' # incomplete data will lead to NA in matrix
#' POP <- POP[-c(3, 3), ]
#'
#' # convert tibble to matrix
#' P <- tibble_to_matrix(POP) # denote matrices with single capital letter
#'
#' # perform some matrix magic
#' P[is.na(P)] <- 0
#' Q <- t(solve(t(P) %*% P) %*% t(P))
#'
#' # and back to tibble
#' matrix_to_tibble(Q, POP)
#' @export
tibble_to_matrix <- function(x) {
  stopifnot(is.data.frame(x))
  x <- as_tibble(x)
  stopifnot(ncol(x) == 3)
  cnames <- colnames(x)
  z <- spread_(x, cnames[2], cnames[3]) %>%
    remove_rownames() %>%
    as.data.frame() %>%
    column_to_rownames(cnames[1]) %>%
    as.matrix()
  dimnames(z) <- NULL # another factor 2
  z
}

#' matrix to tibble
#' @param X a matrix
#' @param template a template
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
#'
matrix_to_tibble <- function(X, template) {
  stopifnot(is.matrix(X))
  stopifnot(is_tibble(template))

  # make sure dimension are correct
  stopifnot(identical(
    dim(tibble_to_matrix(template)),
    dim(X)
  ))

  rownames(X) <- unique(template[[1]])
  colnames(X) <- unique(template[[2]])

  y.lab <- colnames(template)[1]
  x.lab <- colnames(template)[2]
  value.lab <- colnames(template)[3]

  df <- as.data.frame(X) %>%
    rownames_to_column(y.lab) %>%
    as_tibble() %>%
    # requires latest dplyr, tidyr ect
    # gather(!! x.lab, !! value.lab, 2:ncol(.))
    # requires latest dplyr, tidyr ect
    gather(key, value, 2:ncol(.)) %>%
    setNames(c(y.lab, x.lab, value.lab))

  # convert to orginal classes
  as_class <- function(class) {
    get(paste0("as.", class))
  }

  df[[1]] <- as_class(class(template[[1]]))(df[[1]])
  df[[2]] <- as_class(class(template[[2]]))(df[[2]])

  df
}


#' Collect objects
#'
#' @param pattern set of characters or numbers creating a pattern
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
collect_objects <- function(pattern = "^OPT_DELTA") {
  penv <- parent.frame()
  objs.names <- ls(pattern = pattern, envir = penv)
  if (length(objs.names) == 0) {
    stop("no tibble matching pattern: ", pattern)
  }
  objs <- mget(objs.names, envir = penv)
  not.a.df <- !unlist(lapply(objs, inherits, "data.frame"))
  if (any(not.a.df)) {
    stop(
      "Some 'OPT_' objects are not of class 'data.frame', discarding them:",
      paste(names(not.a.df[not.a.df]), collapse = ", ")
    )
    objs <- objs[!not.a.df]
  }
  tidylist_ensure(objs)
}

#' Generate codes to assign tidylist explicitly
#'
#' @param tl tidylist
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
code_tidylist_assign <- function(tl) {
  library(clipr)
  tl.name <- deparse(substitute(tl))
  tl <- tidylist_ensure(tl)
  z <- paste0(names(tl), " <- ", tl.name, "$", names(tl))
  write_clip(z)
  message("Code written to the clipboard.")
}


#' @title Write parameters
#' @param x parameters tibble to write
#' @param file file to write to
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
write_param <- function(x, file) {
  x %>%
    gather(key, value) %>%
    data.table::fwrite(file = file, sep = ";")
}


#' @title Read a single parameters csv file
#' @param file file
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
#' @import data.table
read_param <- function(file) {
  z0 <- data.table::fread(file = file, sep = ";") %>%
    as_tibble()

  z1 <- spread(z0, key, value, convert = TRUE)

  if (identical(dim(z1), c(0L, 0L))) return(z1)

  dplyr::select(z1, one_of(z0[["key"]]))
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


## ==//=================================================================#
#   Funktion:   rente_ram                                               #
## ==\\=================================================================#
#' rente_ram
#' @description Berechnung der monatlichen Renten Skala 44 abhaengig von ram
#'    nach Art. 34 AHVG
#' @param       ram           = durchschnittliches Jahreseinkommen
#' @param       mr            = Hoehe des Mindestbetrages
#' @param       fest1         = fester Anteil bis zum Knick
#' @param       variabel1     = variabler Anteil bis zum Knick
#' @param       fest2         = fester Anteil ueber dem Knick
#' @param       variabel2     = variabler Anteil ueber dem Knick
#' @param       stufe         = Stufe als Faktor von mr (Knickpunkt) fuer Parameter1/2
#' @return      rente         = skalar mit den Rentenhoehe
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @examples
#' plot(seq(0, 100000, 1000), sapply(seq(0, 100000, 1000), rente_ram), type = "l")
#' @export
rente_ram <- function(ram,
                      mr,
                      fest1 = 74 / 100,
                      variabel1 =  13 / 600,
                      fest2 = 104 / 100,
                      variabel2 = 8 / 600,
                      stufe = 36) {
  rente_theorique <- numeric()

  rente_theorique <- case_when(

    # 1. RAMD plus petit ou égal à 36 * R0
    ram <= (stufe * mr) ~ as.numeric(fest1 * mr + variabel1 * ram),

    # 2. RAMD plus grand que 36 * R0
    ram > (stufe * mr) ~ as.numeric(fest2 * mr + variabel2 * ram)
  )

  rente <- case_when(
    # Si la rente calculée est plus petite que R0, elles est R0
    rente_theorique <= mr ~ as.numeric(mr),

    # Si la rente calculée est plus grande que 2*R0, elles est 2*R0
    rente_theorique >= (2 * mr) ~ as.numeric(2 * mr),

    TRUE ~ as.numeric(rente_theorique)
  )

  return(rente)
} # rente_ram

## ==//=================================================================#
#   Funktion:   round2                                                  #
## ==\\=================================================================#
#' round2
#' @description #Fonction pour arrondir aux 0.5 plus haut
#' @param x x to round
#' @param n how many numbers after the comma
#' @export
round2 <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^n
  z * posneg
}

## ==//=================================================================#
#   Funktion:   mround                                                  #
## ==\\=================================================================#
#' mround
#' @description #Fonction pour arrondir
#' @param x x to round
#' @param base how many numbers after the comma
#' @export
mround <- function(x, base) {
  base * round(x / base)
}

## ==//=================================================================#
#   Funktion:   truncate_at_n_decimals                                  #
## ==\\=================================================================#
#' truncate_at_n_decimals
#' @description F#truncate number after n decimals without rounding them
#' @param x x to round
#' @param n how many numbers after the comma
#' @export
truncate_at_n_decimals <- function(x, n) {
  x1 <- trunc(x * 10^n + 1E-9) / 10^n

  return(x1)
}

#'@title Function to create a single identifier number
#'@param method_name name of the clustering method
#'@param path directory path
#'@export
clustmeth_identifier_number <- function (method_name, path){

  params_folder_suffix <- gsub("^.+?_", "", basename(path))

  identifier_number <- paste0(
    "clusters",
    format(Sys.time(), format = "%Y%m%d%H%M%S"),
    "_",
    tolower(Sys.getenv("USER")),
    "_",
    params_folder_suffix

  )
}
