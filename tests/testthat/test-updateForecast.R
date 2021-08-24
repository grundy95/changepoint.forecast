test_that("update from a call to cptSeqCUSUM works", {
  model = cptSeqCUSUM(rnorm(500), m=300)
  ans = updateForecast(rnorm(10,1), model=model)
  expect_length(ans@cusum, 210)
  expect_length(ans@errors, 510)
  expect_length(ans@threshold, 210)
})

test_that("different detectors work",{
  modelCUSUM = cptSeqCUSUM(rnorm(500), m=300, detector='CUSUM')
  modelCUSUM1 = cptSeqCUSUM(rnorm(500), m=300, detector='CUSUM1')
  modelPageCUSUM = cptSeqCUSUM(rnorm(500), m=300, detector='PageCUSUM')
  modelPageCUSUM1 = cptSeqCUSUM(rnorm(500), m=300, detector='PageCUSUM1')

  ansCUSUM = updateForecast(rnorm(10,1), model=modelCUSUM)
  expect_length(ansCUSUM@cusum, 210)
  expect_length(ansCUSUM@errors, 510)
  expect_length(ansCUSUM@threshold, 210)

  ansCUSUM1 = updateForecast(rnorm(10,1), model=modelCUSUM1)
  expect_length(ansCUSUM1@cusum, 210)
  expect_length(ansCUSUM1@errors, 510)
  expect_length(ansCUSUM1@threshold, 210)

  ansPage = updateForecast(rnorm(10,1), model=modelPageCUSUM)
  expect_length(ansPage@cusum, 210)
  expect_length(ansPage@errors, 510)
  expect_length(ansPage@threshold, 210)

  ansPage1 = updateForecast(rnorm(10,1), model=modelPageCUSUM1)
  expect_length(ansPage1@cusum, 210)
  expect_length(ansPage1@errors, 510)
  expect_length(ansPage1@threshold, 210)
})

test_that("different forecast error types works",{
  modelBoth = cptForecast(rnorm(500), m=300)
  modelRaw = cptForecast(rnorm(500), m=300, forecastErrorType='Raw')
  modelSquared = cptForecast(rnorm(500), m=300, forecastErrorType='Squared')

  ansBoth = updateForecast(rnorm(10), modelBoth)
  ansRaw = updateForecast(rnorm(10), modelRaw)
  ansSquared = updateForecast(rnorm(10), modelSquared)

  expect_false(any(is.na(ansBoth@cusum))||any(is.na(ansBoth@cusum2))||any(is.na(ansBoth@threshold))||any(is.na(ansBoth@threshold2))||any(is.na(ansBoth@tau))||any(is.na(ansBoth@tau2)))
  expect_true(is.na(ansRaw@cusum2)&&is.na(ansRaw@threshold2)&&is.na(ansRaw@tau2))
  expect_true(is.na(ansSquared@cusum)&&is.na(ansSquared@threshold)&&is.na(ansSquared@tau))
})

test_that('Error checking - newErrors',{
  ans = cptForecast(stats::rnorm(100, m=40))
  expect_error(updateForecast(c('1','2','3','4','5','6','7'), model=ans),
               "newErrors should be a vector of numeric values with no NA values")
  expect_error(updateForecast(c(stats::rnorm(99), NA_real_), model=ans),
               "newErrors should be a vector of numeric values with no NA values")
  expect_error(updateForecast(c(stats::rnorm(99), Inf), model=ans),
               "newErrors should be a vector of numeric values with no NA values")
})

test_that('Error checking - model',{
  ans = cptForecast(stats::rnorm(100, m=40))
  expect_error(updateForecast(1, model=list()))
})

