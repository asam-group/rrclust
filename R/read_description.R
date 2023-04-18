#' @title Search for invalid lines in UTF-8
#' @param x object
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
invalid_utf8 <- function(x) {
  which(!is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")))
}

#' @title Read / write files encoded in UTF-8
#'
#' @description Read or write files, assuming they are encoded in UTF-8. \code{read_utf8()}
#' is roughly \code{readLines(encoding = 'UTF-8')} (a warning will be issued if
#' non-UTF8 lines are found), and \code{write_utf8()} calls
#' \code{writeLines(enc2utf8(text), useBytes = TRUE)}.
#'
#' @param con A connection or a file path.
#' @param error Whether to signal an error when non-UTF8 characters are detected
#'   (if \code{FALSE}, only a warning message is issued).
#' @param text A character vector (will be converted to UTF-8 via
#'   \code{\link{enc2utf8}()}).
#' @param ... Other arguments passed to \code{\link{writeLines}()} (except
#'   \code{useBytes}, which is \code{TRUE} in \code{write_utf8()}).
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
read_utf8 <- function(con, error = FALSE) {
  x <- readLines(con, encoding = "UTF-8", warn = FALSE)
  x <- trimws(x) # Removes leading and/or trailing whitespace from strings
  i <- invalid_utf8(x)
  n <- length(i)
  if (n > 0) {
    (if (error) stop else warning)(
      if (is.character(con)) c("The file ", con, " is not encoded in UTF-8. "),
      "These lines contain invalid UTF-8 characters: ",
      paste(c(head(i), if (n > 6) "..."), collapse = ", ")
    )
  }
  x
}

# Add description to PARAM_GLOBAL
#' @title read the path fo the data
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @export
read_description <- function(path_container) {
  path.to.description <- normalizePath(
    file.path(path_container, "description"),
    mustWork = TRUE
  )

  txt <- read_utf8(path.to.description)

  # Le titre est donné en 3 langues dans les 3 premières lignes de la description
  titles <- setNames(txt[1:3], c("title_d", "title_f", "title_i"))

  # Pour entrer un sous-titre, mettre "<<subtitle>>" au début de la phrase
  if (unique(grepl("<<subtitle>>", txt[5:7])) == TRUE) {
    subtitles_full <- setNames(txt[5:7], c("subtitle_d", "subtitle_f", "subtitle_i"))
    subtitles <- gsub("<<subtitle>>", "", subtitles_full)
    descr0 <- txt[-(1:7)]
  } else {
    subtitles <- setNames(rep("", 3), c("subtitle_d", "subtitle_f", "subtitle_i"))
    descr0 <- txt[-(1:3)]
  }

  descr <- paste(descr0[descr0 != ""], collapse = "\n")

  as_tibble(c(
    as.list(titles),
    as.list(subtitles)
  )) |>
    mutate(description = descr)
}
#' @title read the path fo the data
#' @export
read_path_data <- function(path_container) {
  path.to.inp <- normalizePath(
    file.path(path_container, "inp"),
    mustWork = TRUE
  )
  read_utf8(path.to.inp)
}
