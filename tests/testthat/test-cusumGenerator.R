test_that("cusumGenerator returns a function/closure", {
  X=stats::rnorm(500)
  expect_type(cusumGenerator(X, m=300), "closure")
  expect_type(cusumGenerator(X, m=300, oneSidedAlt=TRUE), "closure")
})

test_that("cusumGenerator errors correctly", {
  X=stats::rnorm(100)
  expect_error(cusumGenerator(X, m=300),
               'Training sample must be smaller than size of data')
})

test_that("cusumGenerator function works as expected",{
  X=stats::rnorm(500)
  fun = cusumGenerator(X, m=300)
  expect_type(fun(50), "double")
  expect_error(fun(300), 'k too large')
})
