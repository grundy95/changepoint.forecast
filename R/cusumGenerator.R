#' Creates a function for calculating CUSUM statistic
#'
#' Creates a function which returns the CUSUM statistic at a given time point $k$.
#'
#' @param X Time series
#' @param m Length of training period
#' @param oneSidedAlt Logical. Testing the one-sided or two-sided alternative
#'
#' @return A function with one parameter $k$.


cusumGenerator = function(X, m, oneSidedAlt=FALSE){
  n = length(X)
  if(m >= n){
    stop('Training sample must be smaller than size of data')
  }
  forwardCumsum = purrr::accumulate(X, `+`)

  function(k){
    if(k>(n-m)){
      stop('k too large')
    }
    Q = forwardCumsum[[m + k]] - ((k+m)/m) * forwardCumsum[[m]]
    if(oneSidedAlt){
      return(Q)
    }else{
      return(abs(Q))
    }
  }
}
