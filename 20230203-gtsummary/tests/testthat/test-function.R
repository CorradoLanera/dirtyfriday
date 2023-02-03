test_that("import data works", {
  expect_tibble(import_data())
})


test_that("get_var_id works", {
  expect_equal(get_var_id("amr_cathegory_p_s6rp_1"), 1)
  expect_equal(get_var_id("amr_cathegory_p_s6rp_9"), 9)
})

test_that("get_sym_id works", {
  expect_equal(get_sym_id("amr_cathegory_p_s6rp_1"), "≥")
  expect_equal(get_sym_id("amr_cathegory_p_s6rp_9"), "=")
  expect_equal(
    get_sym_id(
      c("amr_cathegory_p_s6rp_1", "amr_cathegory_p_s6rp_9")
    ),
    c("≥", "=")
  )
})

test_that("convert_to_improvement_db works", {
  # setup
  transformed_db <- convert_to_improvement_db(import_data())

  expected_var_names <- paste(c(rep("≥", 8), "="), 1:9)
  expect_equal(names(transformed_db), expected_var_names)
})

test_that("produce_dia_prop_db works", {
  # setup
  res <- import_data() |>
    convert_to_improvement_db() |>
    produce_dia_prop_db()

  # test
  expect_tibble(res, any.missing = FALSE, ncols = 3)
  expect_set_equal(names(res), c("name", "prop", "diagnosis"))

})



test_that("plot remain the same", {
  local_edition(3)

  # setup
  db <- import_data() |>
    convert_to_improvement_db() |>
    produce_dia_prop_db()

  expect_snapshot_value(gg_diagnoses(db), style = "serialize")
})
