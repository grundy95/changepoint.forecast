test_that("weight function works", {
  expect_equal(weightFun(m=300, k=100, gamma=0.2), 17.502, tolerance=0.001)
  expect_error(weightFun(m=100, k=100, gamma=-0.2), "gamma must be between 0 and 1")
})

test_that("cptSeqCUSUM output is correct class", {
  set.seed(1)
  X = stats::rnorm(500)
  expect_s4_class(cptSeqCUSUM(X, m=300, Class=TRUE), "cptFor")
  expect_type(cptSeqCUSUM(X, m=300, Class=FALSE), "list")
})

test_that("Different detectors run",{
  X = stats::rnorm(500)
  Page1 = cptSeqCUSUM(X, m=300, detector="PageCUSUM1")
  expect_equal(Page1@detector, "PageCUSUM1")

  Page2 = cptSeqCUSUM(X, m=300, detector="PageCUSUM")
  expect_equal(Page2@detector, "PageCUSUM")

  Cusum1 = cptSeqCUSUM(X, m=300, detector="CUSUM1")
  expect_equal(Cusum1@detector, "CUSUM1")

  Cusum2 = cptSeqCUSUM(X, m=300, detector="CUSUM")
  expect_equal(Cusum2@detector, "CUSUM")

  expect_error(cptSeqCUSUM(X, m=300, detector="Normal"),
               'changepoint detector not supported.
         Please choose between "PageCUSUM", "PageCUSUM1", "CUSUM" or "CUSUM1"')
})

test_that("Different critVals run",{
  X = stats::rnorm(500)
  Lookup = cptSeqCUSUM(X, m=300, critValue='Lookup')
  expect_s4_class(Lookup, "cptFor")

  Simulate = cptSeqCUSUM(X, m=300, critValue='Simulate',
                         detector='CUSUM1', samples=100, npts=50)
  expect_s4_class(Simulate, "cptFor")

  Numeric = cptSeqCUSUM(X, m=300, critValue=1)
  expect_s4_class(Numeric, "cptFor")

  expect_error(cptSeqCUSUM(X, m=300, critValue='CROPS'),
               'critValue should either be "Lookup", "Simulate" or a single number containing the critical value to be used')
})

test_that("Tau is correct",{
  set.seed(1)
  X = stats::rnorm(500)
  NoChange = cptSeqCUSUM(X, m=300, critValue=100000)
  expect_equal(NoChange@tau, Inf)

  Xalt = c(X[1:300], X[301:500]+10)
  Change = cptSeqCUSUM(Xalt, m=300)
  expect_true(Change@tau<Inf)
})


