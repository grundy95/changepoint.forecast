test_that("basic functionality, no trend", {
  set.seed(1)
  fullData = stats::arima.sim(list(ar=c(0.3,0.6,-0.2), ma=c(0.8,-0.3)), n=100)
  trainData = fullData[1:50]
  X = fullData[51:100]
  expect_s4_class(forecastMlp(X=X, m=300, trainData=trainData), "cptFor")
  expect_type(forecastMlp(X=X, m=300, trainData=trainData, Class=FALSE), "list")
})

test_that("basic functionality, with trend", {
  set.seed(1)
  fullData = stats::arima.sim(list(ar=c(0.3,0.6,-0.2), ma=c(0.8,-0.3)), n=100)+seq(0,30, length.out=100)
  trainData = fullData[1:50]
  X = fullData[51:100]
  expect_s4_class(forecastMlp(X=X, m=300, trainData=trainData, trend=TRUE), "cptFor")
  expect_type(forecastMlp(X=X, m=300, trainData=trainData, trend=TRUE, Class=FALSE), "list")
})
