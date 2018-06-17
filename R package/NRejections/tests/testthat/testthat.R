library(testthat)
library(devtools)


###################### EVALUE: ANNALS PAPER EXAMPLES ###################### 

test_that("Annals case #1", {
  expect_equal( 3.9 + sqrt( 3.9 * (3.9 - 1) ), evalues.RR( 3.9, 1.8, 8.7 )[2, "point"] )
  expect_equal( 1.8 + sqrt( 1.8 * (1.8 - 1) ), evalues.RR( 3.9, 1.8, 8.7 )[2, "lower"] )
  expect_identical( is.na(NA), is.na(evalues.RR( 3.9, 1.8, 8.7 )[2, "upper"]) )
})

