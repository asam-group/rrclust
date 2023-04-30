test_that("path of data is returned as character", {
  expect_equal(typeof(gen_demo_data()), "character")
})

test_that("default nrow is 1000", {
  expect_equal(nrow(as_tibble(tidylist_read(gen_demo_data()))), 1000)
})
