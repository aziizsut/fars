library(fars)

context("Making a file name")

test_that("It will produce file name with the correct year",{
  expect_equal(make_filename(2016), "accident_2016.csv.bz2")
  expect_equal(make_filename(2017), "accident_2017.csv.bz2")
})
