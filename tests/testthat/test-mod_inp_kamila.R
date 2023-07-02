test_that("path_param works", {
  path <- "params_kamila_large"
  expect_equal(path_param(path), path)
})


test_that("all necessary inputs are read", {
  path_random_data <- gen_demo_data()
  demo_data <- tidylist_read(path_random_data)
  path <- "params_kamila_large"
  tl_PARAM_GLOBAL <- param_tidylist_read(path)

  # replace empty variable with the temporary path
  tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- str_remove(
    dirname(path_random_data), "/all"
  )

  PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
    mutate_all(as.character) |>
    pivot_longer(
      everything(),
      names_to = "key",
      values_to = "value"
    )

  tidylist_write(tidylist(PARAM_GLOBAL), path = path)
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")
  expect_equal(
    names(tl_inp_kamila),
    c("IND_YEARLY_RR", "PARAM_GLOBAL", "path_param_folder", "PARAM_KAMILA")
  )
})


test_that("all necessary variables of IND_YEARLY_RR are read", {
  path_random_data <- gen_demo_data()
  demo_data <- tidylist_read(path_random_data)
  path <- "params_kamila_large"
  tl_PARAM_GLOBAL <- param_tidylist_read(path)

  # replace empty variable with the temporary path
  tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- str_remove(
    dirname(path_random_data), "/all"
  )

  PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
    mutate_all(as.character) |>
    pivot_longer(
      everything(),
      names_to = "key",
      values_to = "value"
    )

  tidylist_write(tidylist(PARAM_GLOBAL), path = path)
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")

  expect_equal(
    names(tl_inp_kamila$IND_YEARLY_RR),
    c(
      "alt", "sex", "nat", "dom", "gpr", "zv", "csplit", "cplaf",
      "jahr", "ram", "monatliche_rente", "age_ret", "eprc", "lcot",
      "lcotg", "lbedu", "lbass"
    )
  )
})


test_that("all necessary variables of PARAM_GLOBAL are read", {
  path_random_data <- gen_demo_data()
  demo_data <- tidylist_read(path_random_data)
  path <- "params_kamila_large"
  tl_PARAM_GLOBAL <- param_tidylist_read(path)

  # replace empty variable with the temporary path
  tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- str_remove(
    dirname(path_random_data), "/all"
  )

  PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
    mutate_all(as.character) |>
    pivot_longer(
      everything(),
      names_to = "key",
      values_to = "value"
    )

  tidylist_write(tidylist(PARAM_GLOBAL), path = path)
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")

  expect_equal(
    names(tl_inp_kamila$PARAM_GLOBAL),
    c(
      "method_name", "path_data", "description", "pct_sample_ts",
      "categ_var", "cont_var", "identifier_number"
    )
  )
})


test_that("all necessary variables of PARAM_KAMILA are read", {
  path_random_data <- gen_demo_data()
  demo_data <- tidylist_read(path_random_data)
  path <- "params_kamila_large"
  tl_PARAM_GLOBAL <- param_tidylist_read(path)
  tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- str_remove(
    dirname(path_random_data), "/all"
  )

  PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
    mutate_all(as.character) |>
    pivot_longer(
      everything(),
      names_to = "key",
      values_to = "value"
    )

  # rewrite PARAM_GLOBAL with the demo data path
  tidylist_write(tidylist(PARAM_GLOBAL), path = path)
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")

  expect_equal(
    names(tl_inp_kamila$PARAM_KAMILA),
    c(
      "calc_kstar", "numberofclusters", "numinit", "maxiter", "calcnumclust",
      "pred_threshold", "param_kstar", "categ_var_expl", "cont_var_expl",
      "path_inp_kmres"
    )
  )
})
