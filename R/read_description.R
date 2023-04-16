# Add description to PARAM_GLOBAL
#' @title read the path fo the data
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
