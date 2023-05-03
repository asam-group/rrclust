test_that("columns of RR_OASI are correctly renamed", {
  # generate random demo data
  path_random_data <- gen_demo_data()

  # read the demo data
  demo_data <- tidylist_read(path_random_data)

  path <- "params_kamila_large"

  # read PARAM_GLOBAL
  tl_PARAM_GLOBAL <- param_tidylist_read(path)

  # replace empty variable with the temporary path
  tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- str_remove(
    dirname(path_random_data), "/all"
  )

  PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(
      everything(),
      names_to = "key",
      values_to = "value"
    )

  # rewrite PARAM_GLOBAL with the demo data path
  tidylist_write(tidylist(PARAM_GLOBAL), path = path)

  # input
  tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")

  # prepare pension register
  tl_prepa_rr <- mod_prepa_rr(tl_inp_kamila$IND_YEARLY_RR)

  # test
  expect_equal(
    names(tl_prepa_rr$RR_OASI),
    c(
      "age", "sex", "nat", "resid", "splitting", "capping", "year",
      "aadr", "monthly_pension", "age_ret", "eprc", "contrib_m_ind",
      "contrib_y_ageclass", "bonus_m_edu", "bonus_m_assist", "benef_type",
      "marital_stat", "scale", "marital_stat1", "marital_stat2", "marital_stat3",
      "marital_stat4", "benef_type1", "benef_type2", "benef_type3",
      "benef_type4", "benef_type5", "benef_type6", "benef_type7", "benef_type8",
      "age_retire"
    )
  )
})
