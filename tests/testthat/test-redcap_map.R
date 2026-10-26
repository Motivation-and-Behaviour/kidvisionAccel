test_that("redcap_fields() lists every day field for days 1 to 10", {
  fields <- redcap_fields()
  derived <- fields$field[fields$role == "derived"]

  expect_length(grep("^cac_day[0-9]+_", derived), 16 * 10)
  expect_true(all(c("cac_day1_calday", "cac_day10_eff") %in% derived))
  expect_false(any(grepl("{n}", derived, fixed = TRUE)))
})

test_that("redcap_fields() has no duplicates", {
  expect_equal(anyDuplicated(redcap_fields()$field), 0)
})

test_that("staff-entered fields are never written", {
  fields <- redcap_fields()
  written <- fields$field[fields$role != "read"]

  expect_false(any(c("cac_gen_wrist", "cac_gen_num") %in% written))
})
