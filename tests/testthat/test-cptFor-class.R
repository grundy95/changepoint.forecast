test_that("plot/show/summary functionality", {
  set.seed(1)
  ans = cptForecast(stats::rnorm(500), m=300)
  ans2 = cptForecast(stats::rnorm(500), m=300, forecastErrorType='Raw')
  ans3 = cptForecast(stats::rnorm(500), m=300, forecastErrorType='Squared')
  ans4 = cptForecast(c(stats::rnorm(400), stats::rnorm(100,5)), m=300)
  ans5 = cptForecast(c(stats::rnorm(400), stats::rnorm(100,5)), m=300, forecastErrorType='Raw')
  ans6 = cptForecast(c(stats::rnorm(400), stats::rnorm(100,5)), m=300, forecastErrorType='Squared')
  ans7 = cptForecast(c(stats::rnorm(400), stats::rnorm(100,0,3)), m=300)
  expect_error(plot(new("cptFor")), 'cusum or cusum2 slots needs to be filled to plot')
  expect_s3_class(plot(ans), "ggplot")
  expect_s3_class(plot(ans2), "ggplot")
  expect_s3_class(plot(ans3), "ggplot")
  expect_s3_class(plot(ans4), "ggplot")
  expect_s3_class(plot(ans5), "ggplot")
  expect_s3_class(plot(ans6), "ggplot")
  expect_s3_class(plot(ans7), "ggplot")
  expect_type(invisible(capture.output(show(ans))), 'character')
  expect_type(invisible(capture.output(summary(ans))), 'character')
  expect_type(invisible(capture.output(show(ans2))), 'character')
  expect_type(invisible(capture.output(summary(ans2))), 'character')
  expect_type(invisible(capture.output(show(ans3))), 'character')
  expect_type(invisible(capture.output(summary(ans3))), 'character')
  expect_type(invisible(capture.output(show(ans4))), 'character')
  expect_type(invisible(capture.output(summary(ans4))), 'character')
  expect_type(invisible(capture.output(show(ans5))), 'character')
  expect_type(invisible(capture.output(summary(ans5))), 'character')
  expect_type(invisible(capture.output(show(ans6))), 'character')
  expect_type(invisible(capture.output(summary(ans6))), 'character')
  expect_type(invisible(capture.output(show(ans7))), 'character')
  expect_type(invisible(capture.output(summary(ans7))), 'character')
})


test_that("retrieval functions are working",{
  ans = cptForecast(rnorm(500), m=300)
  ans@errors <- rnorm(500)
  expect_type(errors(ans), "double")
  ans@m <- 30
  expect_type(m(ans), "double")
  ans@detector <- "CUSUM"
  expect_type(detector(ans), "character")
  ans@gamma <- 0.25
  expect_type(gamma(ans), "double")
  ans@errorsVar <- 2
  expect_type(errorsVar(ans), "double")
  ans@errors2Var <- 2
  expect_type(errors2Var(ans), "double")
  ans@cusum <- rnorm(200)
  expect_type(cusum(ans), "double")
  ans@cusum2 <- rnorm(200)
  expect_type(cusum2(ans), "double")
  ans@alpha <- 0.01
  expect_type(alpha(ans), "double")
  ans@threshold <- rep(20, 200)
  expect_type(threshold(ans), "double")
  ans@threshold2 <- rep(20, 200)
  expect_type(threshold2(ans), "double")
  ans@tau <- Inf
  expect_type(tau(ans), "double")
  ans@tau2 <- Inf
  expect_type(tau2(ans), "double")
})
