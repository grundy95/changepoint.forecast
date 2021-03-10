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
