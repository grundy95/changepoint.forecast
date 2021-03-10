#' Creates a function for calculating Page's CUSUM statistic
#'
#' Creates a function which returns Page's CUSUM statistic at a given time point $k$.
#'
#' @param X Time series
#' @param m Length of training period
#' @param oneSidedAlt Logical. Testing the one-sided or two-sided alternative
#'
#' @return A function with one parameter $k$

cusumPageGenerator = function(X, m, oneSidedAlt = FALSE){
  n = length(X)
  if(m >= n){
    stop('Training sample must be smaller than size of data')
  }
  cusum = cusumGenerator(X, m, oneSidedAlt=TRUE)
  cusumVals = purrr::map_dbl(1:(n-m),cusum)

  function(k){
    if(k>(n-m)){
      stop('k too large')
    }
    if(oneSidedAlt){
      return(cusumVals[k] - min(cusumVals[1:k]))
    }else{
      max(abs(cusumVals[k]-cusumVals[1:k]))
    }
  }
}
