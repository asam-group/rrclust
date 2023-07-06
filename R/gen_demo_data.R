#' Generate random demo data
#' 
#' Generate random demo data and write into a csv file.
#' @param path Path where to store the demo data. Default: `tempdir()`.
#' @param method_name Name of the clustering method.
#' @param data_size Size of the simulated data frame. Default: 1000 rows.
#' @return The path of the csv file.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @autoglobal
#' @export
#'
#' @examples gen_demo_data()
gen_demo_data <- function(path = tempdir(),
                          method_name = "kamila",
                          data_size = 1000) {
  data_size <- data_size

  IND_YEARLY_RR <- tibble(
    alt = sample(0:99, size = data_size, replace = TRUE),
    sex = sample(c("f", "m"), replace = TRUE, size = data_size),
    nat = sample(c("au", "ch"), replace = TRUE, size = data_size),
    dom = sample(c("au", "ch"), replace = TRUE, size = data_size),
    gpr = sample(c(
      "rvieillesse_simple",
      "rveuve",
      "rorphelin_pere_simple",
      "rorphelin_mere_simple",
      "rorphelin_double",
      "rcompl_femme",
      "renfant_pere_simple",
      "renfant_mere_simple"
    ), replace = TRUE, size = data_size),
    zv = case_when(
      alt <= 18 ~ "ledig", # single
      TRUE ~ sample(c(
        "geschieden",
        "ledig",
        "verheiratet",
        "verwitwet"
      ), replace = TRUE, size = data_size)
    ),
    csplit = case_when(
      alt <= 62 ~ NA_integer_, # before 62, no possible splitting
      TRUE ~ sample(c(0, 1), replace = TRUE, size = data_size)
    ),
    cplaf = case_when(
      alt <= 62 ~ NA_integer_, # before 62, no possible capping
      TRUE ~ sample(c(0, 1), replace = TRUE, size = data_size)
    ),
    jahr = format(Sys.time(), "%Y"),
    ram = sample(10e4:10e6, replace = TRUE, size = data_size),
    monatliche_rente = sample(1:4000, replace = TRUE, size = data_size),
    age_ret = case_when(
      alt <= 62 ~ NA_integer_, # before 62, no possible retirement
      TRUE ~ sample(62:70, replace = TRUE, size = data_size)
    ),
    eprc = sample(1 / 44:1, replace = TRUE, size = data_size),
    lcot = sample(0:500, replace = TRUE, size = data_size),
    lcotg = sample(0:44, replace = TRUE, size = data_size),
    lbedu = case_when(
      alt <= 62 ~ NA_integer_, # before 62, no possible bonus for education
      TRUE ~ sample(0:500, replace = TRUE, size = data_size)
    ),
    lbass = case_when(
      alt <= 62 ~ NA_integer_, # before 62, no possible bonus for assistance
      TRUE ~ sample(0:500, replace = TRUE, size = data_size)
    )
  )

  new_path_all <- file.path(path, "all")
  if (!file.exists(new_path_all)) {
    dir.create(new_path_all, recursive = TRUE)
  }

  new_path_method <- file.path(path, method_name)
  if (!file.exists(new_path_method)) {
    dir.create(new_path_method, recursive = TRUE)
  }

  tidylist_write(tidylist(IND_YEARLY_RR), path = new_path_all)

  file.path(new_path_all, "IND_YEARLY_RR.csv")
}
