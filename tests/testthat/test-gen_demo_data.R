test_that("path of data is returned as character", {
  expect_equal(typeof(gen_demo_data()), "character")
})

test_that("default nrow is 1000", {
  expect_equal(nrow(tidylist_read(gen_demo_data())$IND_YEARLY_RR), 1000)
})


test_that("same data are generated", {
  set.seed(42)
  IND_YEARLY_RR <- tidylist_read(gen_demo_data())$IND_YEARLY_RR
  expect_equal(sum(IND_YEARLY_RR$ram, na.rm = TRUE), 4956854067)
})


test_that("all variables are generated", {
  IND_YEARLY_RR <- tidylist_read(gen_demo_data())$IND_YEARLY_RR
  expect_equal(
    names(IND_YEARLY_RR),
    c(
      "alt", "sex", "nat", "dom", "gpr", "zv", "csplit", "cplaf",
      "jahr", "ram", "monatliche_rente", "age_ret", "eprc", "lcot",
      "lcotg", "lbedu", "lbass"
    )
  )
})
