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
               "Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
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
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
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

test_that("Error checking - X",{
  expect_error(cptSeqCUSUM(c('1','2','3','4','5','6','7'), m=4),
               "X should be a vector of numeric values with no NA values")
  expect_error(cptSeqCUSUM(c(stats::rnorm(99), NA_real_), m=40),
               "X should be a vector of numeric values with no NA values")
  expect_error(cptSeqCUSUM(c(stats::rnorm(99), Inf), m=40),
               "X should be a vector of numeric values with no NA values")
})

test_that("Error checking - m",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = c(20,40)),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = 6.5),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = -20),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = 150),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = '20'),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m = NA_real_),
               "m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
})

test_that("Error checking - detector",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, detector='Cusum'),
               "Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, detector=5),
               "Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
})

test_that("Error checking - gamma",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, gamma=c(0.2, 0.4)),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, gamma=-0.2),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, gamma=0.5),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, gamma='0.2'),
               "gamma should be a single positive number such that 0<=gamma<0.5")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, gamma=NA_real_),
               "gamma should be a single positive number such that 0<=gamma<0.5")
})


test_that("Error checking - sigma2",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, sigma2=c(0.2, 0.4)),
               "sigma2 should be a single positive number greater than 0")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, sigma2=-0.2),
               "sigma2 should be a single positive number greater than 0")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, sigma2=Inf),
               "sigma2 should be a single positive number greater than 0")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, sigma2='0.2'),
               "sigma2 should be a single positive number greater than 0")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, sigma2=NA_real_),
               "sigma2 should be a single positive number greater than 0")
})
test_that("Error checking - critValue",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, critValue='5'),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, critValue=-20),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, critValue=Inf),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, critValue=NA_real_),
               "critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
})

test_that("Error checking - alpha",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, alpha='0.2'),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, alpha=-1),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, alpha=Inf),
               "alpha should be a single positive number such that 0<=alpha<=1")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, alpha=NA_real_),
               "alpha should be a single positive number such that 0<=alpha<=1")
})

test_that("Error checking - samples",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, samples = 19),
               "samples should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, samples = '25'),
               "samples should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, samples = Inf),
               "samples should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, samples = NA_real_),
               "samples should be a single positive integer greater than 20")
})

test_that("Error checking - npts",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, npts = 19),
               "npts should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, npts = '25'),
               "npts should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, npts = Inf),
               "npts should be a single positive integer greater than 20")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, npts = NA_real_),
               "npts should be a single positive integer greater than 20")
})

test_that("Error checking - Class",{
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, Class = NA_real_),
               "Class should be logical, TRUE or FALSE")
  expect_error(cptSeqCUSUM(stats::rnorm(100), m=40, Class = 6),
               "Class should be logical, TRUE or FALSE")
})
