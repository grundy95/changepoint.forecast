#' Forecast with Auto ARIMA
#'
#' Calculates one-step-ahead forecast errors from an automatically chosen ARIMA model based upon training data
#'
#' @param X time series. Can be numeric vector or a `ts` object
#' @param m length of training period
#' @param trend Logical. Include a deterministic linear trend.
#' @param Class Logical. Return an object of class `cptFor`.
#' @param ... Additional parameters to pass to `auto.arima`.
#'
#' @return object of class `cpt.for`. If `Class=FALSE` a list with elements:
#' \itemize{
#'   \item{model: }{fitted ARIMA model based upon training data}
#'   \item{errors: }{forecast errors}
#' }
#' @export
#'
#' @examples
#' X = stats::arima.sim(list(ar=0.3), n=500)
#' ans = forecastAutoArima(X, m=300)
#' summary(ans)
forecastAutoArima = function(X, m, trend=FALSE, Class=TRUE, ...){
  if(class(X)!='ts'){
    X = ts(X)
  }
  if(trend){
    model = forecast::auto.arima(subset(X, end=m), xreg=1:m, d=0, D=0, ...)
    forecastErrors = forecast::Arima(X, model=model, xreg=1:n)$residuals
  }else{
    model = forecast::auto.arima(subset(X, end=m), ...)
    forecastErrors = forecast::Arima(X, model=model)$residuals
  }
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
