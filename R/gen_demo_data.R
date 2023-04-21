#' @title Generate demo data
#' @description Generate demo data and write into a csv file
#' @param path path where to store the demo data. Default: `tempdir()`.
#' @param method_name name of the clustering method.
#' @return the path of the csv file
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @autoglobal
#' @export
#'
#' @examples gen_demo_data()
gen_demo_data <- function(path = tempdir(),
                          method_name = "kamila") {
  # set the seed
  set.seed(42)

  # set the data size
  data_size <- 1000

  # generate demo data
  demo_data <- tibble(
    age = sample(0:99, data_size, replace = TRUE),
    sex = sample(c(0, 1), replace = TRUE, size = data_size),
    nat = sample(c(0, 1), replace = TRUE, size = data_size),
    resid = sample(c(0, 1), replace = TRUE, size = data_size),
    benef_type = sample(1:8, replace = TRUE, size = data_size),
    marital_stat = case_when(
      age <= 18 ~ 2L, # single
      TRUE ~ sample(1:4, replace = TRUE, size = data_size)
    ),
    splitting = case_when(
      age <= 62 ~ NA_integer_, # before 62, no possible splitting
      TRUE ~ sample(c(0, 1), replace = TRUE, size = data_size)
    ),
    capping = case_when(
      age <= 62 ~ NA_integer_, # before 62, no possible capping
      TRUE ~ sample(c(0, 1), replace = TRUE, size = data_size)
    ),
    year = format(Sys.time(), "%Y"),
    aadr = sample(10e4:10e6, replace = TRUE, size = data_size),
    monthly_pension = sample(1:4000, replace = TRUE, size = data_size),
    age_retire = case_when(
      age <= 62 ~ NA_integer_, # before 62, no possible retirement
      TRUE ~ sample(62:70, replace = TRUE, size = data_size)
    ),
    scale = sample(0:44, replace = TRUE, size = data_size),
    contrib_m_ind = sample(0:500, replace = TRUE, size = data_size),
    contrib_y_ageclass = sample(0:44, replace = TRUE, size = data_size),
    bonus_m_edu = case_when(
      age <= 62 ~ NA_integer_, # before 62, no possible bonus for education
      TRUE ~ sample(0:500, replace = TRUE, size = data_size)
    ),
    bonus_m_assist = case_when(
      age <= 62 ~ NA_integer_, # before 62, no possible bonus for assistance
      TRUE ~ sample(0:500, replace = TRUE, size = data_size)
    )
  )

  # create an "all" folder
  new_path_all <- file.path(path, "all")
  if (!file.exists(new_path_all)) {
    dir.create(new_path_all, recursive = TRUE)
  }

  # create an method_name folder
  new_path_method <- file.path(path, method_name)
  if (!file.exists(new_path_method)) {
    dir.create(new_path_method, recursive = TRUE)
  }

  # create a csv with the demo data
  tidylist_write(tidylist(demo_data), path = new_path_all)

  # return
  file.path(new_path_all, "demo_data.csv")
}
