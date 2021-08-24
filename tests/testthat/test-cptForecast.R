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
               "forecastErrorType must be one of 'Both', 'Raw', 'Squared'. Note these are case-sensitive")
})

test_that("Error checking - errors",{
  expect_error(cptForecast(c('1','2','3','4','5','6','7'), m=4),
               "Errors should be a vector of numeric values with no NA values")
  expect_error(cptForecast(c(stats::rnorm(99), NA_real_), m=40),
               "Errors should be a vector of numeric values with no NA values")
  expect_error(cptForecast(c(stats::rnorm(99), Inf), m=40),
               "Errors should be a vector of numeric values with no NA values")
})

test_that("Error checking - m",{
  expect_error(cptForecast(stats::rnorm(100), m = c(20,40)),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptForecast(stats::rnorm(100), m = 6.5),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptForecast(stats::rnorm(100), m = -20),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptForecast(stats::rnorm(100), m = 150),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptForecast(stats::rnorm(100), m = '20'),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
    expect_error(cptForecast(stats::rnorm(100), m = NA_real_),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
})

test_that("Error checking - detector",{
  expect_error(cptForecast(stats::rnorm(100), m=40, detector='Cusum'),
               "Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
  expect_error(cptForecast(stats::rnorm(100), m=40, detector=5),
               "Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
})
test_that("Error checking - forecastErrorType",{
  expect_error(cptForecast(stats::rnorm(100), m=40, forecastErrorType ='Normal'),
               "forecastErrorType must be one of 'Both', 'Raw', 'Squared'. Note these are case-sensitive")
  expect_error(cptForecast(stats::rnorm(100), m=40, forecastErrorType=5),
               "forecastErrorType must be one of 'Both', 'Raw', 'Squared'. Note these are case-sensitive")
})
test_that("Error checking - gamma",{
  expect_error(cptForecast(stats::rnorm(100), m=40, gamma=c(0.2, 0.4)),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptForecast(stats::rnorm(100), m=40, gamma=-0.2),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptForecast(stats::rnorm(100), m=40, gamma=0.5),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptForecast(stats::rnorm(100), m=40, gamma='0.2'),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptForecast(stats::rnorm(100), m=40, gamma=NA_real_),
               "gamma should be a single positive number such that 0<=gamma<0.5")
})

test_that("Error checking - critValue",{
  expect_error(cptForecast(stats::rnorm(100), m=40, critValue='5'),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptForecast(stats::rnorm(100), m=40, critValue=-20),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptForecast(stats::rnorm(100), m=40, critValue=Inf),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptForecast(stats::rnorm(100), m=40, critValue=NA_real_),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
})

test_that("Error checking - alpha",{
  expect_error(cptForecast(stats::rnorm(100), m=40, alpha='0.2'),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptForecast(stats::rnorm(100), m=40, alpha=-1),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptForecast(stats::rnorm(100), m=40, alpha=Inf),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptForecast(stats::rnorm(100), m=40, alpha=NA_real_),
               "alpha should be a single positive number such that 0<=alpha<=1")
})

test_that("Error checking - samples",{
  expect_error(cptForecast(stats::rnorm(100), m=40, samples = 19),
               "samples should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, samples = '25'),
               "samples should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, samples = Inf),
               "samples should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, samples = NA_real_),
               "samples should be a single positive integer greater than 20")
})

test_that("Error checking - npts",{
  expect_error(cptForecast(stats::rnorm(100), m=40, npts = 19),
               "npts should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, npts = '25'),
               "npts should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, npts = Inf),
               "npts should be a single positive integer greater than 20")
  expect_error(cptForecast(stats::rnorm(100), m=40, npts = NA_real_),
               "npts should be a single positive integer greater than 20")
})


