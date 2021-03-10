test_that("basic functionality", {
  set.seed(1)
  X = stats::rnorm(500)
  expect_s4_class(forecastAutoArima(X, m=300), "cptFor")
  expect_type(forecastAutoArima(X, m=300, Class=FALSE), "list")
})
