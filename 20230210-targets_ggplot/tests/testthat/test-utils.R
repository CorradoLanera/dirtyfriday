test_that("get_dirtyfriday_file works", {

  expect_file_exists(get_dirtyfriday_file("baseline.csv"))

})
