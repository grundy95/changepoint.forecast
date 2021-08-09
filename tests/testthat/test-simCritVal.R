test_that("limitDistGenerator works for different detectors",{
  for(detector in c("PageCUSUM", "PageCUSUM1", "CUSUM", "CUSUM1")){
    ans1 = limDistGenerator(detector=detector, gamma=0, samples=20, npts=50)
    ans2 = limDistGenerator(detector=detector, gamma=0, samples=20, npts=50, progressBar=FALSE)
    expect_vector(ans1, ptype=double(), size=20)
    expect_vector(ans2, ptype=double(), size=20)
  }
  expect_error(limDistGenerator(detector='Normal'),
               'Detector not recognized. Please choose from "PageCUSUM", PageCUSUM1", "CUSUM" or "CUSUM1"')
})

test_that("simCritVal returns single numeric",{
  ans = simCritVal(samples=20, npts=50)
  expect_vector(ans, double(), 1)
})
