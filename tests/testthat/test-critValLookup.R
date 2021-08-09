test_that("critValLookup succeeds for all combinations",{
  for(gamma in seq(0,0.45,by=0.025)){
    for(alpha in c(0.01,0.05,0.1)){
      for(detector in c("PageCUSUM", "PageCUSUM1", "CUSUM", "CUSUM1")){
        expect_type(critValLookup(gamma=gamma, alpha=alpha, detector=detector), "double")
      }
    }
  }
})

test_that("critValLookup fails as expected",{
  expect_error(critValLookup(gamma=-0.5))
  expect_error(critValLookup(alpha=1.1))
  expect_error(critValLookup(detector='Normal'))
})
