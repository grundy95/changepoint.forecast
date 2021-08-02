test_that("cptForecast output is correct class", {
  set.seed(1)
  X = stats::rnorm(500)
  expect_s4_class(cptForecast(X, m=300), "cptFor")
})

test_that("forecastErrorType is working",{
  X = stats::rnorm(500)
  Raw = cptForecast(X, m=300, forecastErrorType='Raw')
  expect_length(Raw@cusum, 200)
  expect_true(is.na(Raw@cusum2))

  Squared = cptForecast(X, m=300, forecastErrorType='Squared')
  expect_length(Squared@cusum2, 200)
  expect_true(is.na(Squared@cusum))

  expect_error(cptForecast(X, m=300, forecastErrorType='None'),
               'forecastErrorType not supported. Please use one of "Both", "Raw" or "Squared"')
})
