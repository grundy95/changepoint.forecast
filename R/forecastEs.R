#' Forecast with Exponential Smoothing
#'
#' Calculate one-step-ahead forecast errors from an automatically chosen Exponential Smoothing model based upon training data.
#'
#' @param X time series. Can be numeric vector or a `ts` object
#' @param m length of training period
#' @param Model Type of Exponential Smoothing models to allow.
#' @param Class Logical. Return an object of class `cptFor`.
#' @param ... Additional parameters to pass to `es`.
#'
#' @return object of class `cptFor`. If `Class=FALSE` a list with elements:
#' \itemize{
#'   \item{model: }{fitted Exponential Smoothing model based upon training data}
#'   \item{errors: }{forecast errors}
#' }
#' @export
#'
#' @examples
#' #' X = stats::arima.sim(list(ar=c(0.3, 0.1, -0.4), ma=c(0.5, 0.2)), n=500)
#' ans = forecastEs(X, m=300)
#' summary(ans)
forecastEs = function(X, m, Model='XXX', Class=TRUE, ...){
  model = smooth::es(X[1:m], h=1, model=Model, ...)
  forecastErrors = smooth::es(X, model=model, h=1)$residuals
  if(Class){
    return(new('cptFor',
               X=X,
               m=m,
               model=model,
               errors=forecastErrors))
  }else{
    return(list('model'=model, 'errors'=forecastErrors))
  }
}
