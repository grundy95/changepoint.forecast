test_that("cptForecast output is correct class", {
  set.seed(1)
  X = stats::rnorm(500)
  expect_s4_class(cptForecast(X, m=300), "cptFor")
  errors = cptFor(errors=X)
  expect_s4_class(cptForecast(errors, m=300), "cptFor")
})
