#' Forecast with ML
#'
#' Calculates one-step-ahead forecast errors from a machine learning model based upon training data
#'
#' @param X time series. Can be numeric vector or a `ts` object
#' @param m numeric. Length of stable forecasts
#' @param trainData training data. This data is used to train the ML model
#' @param trend Logical. Include a deterministic linear trend.
#' @param Class Logical. Return an object of class `cptFor`.
#' @param ... Additional parameters to pass to `mlp`.
#'
#' @return object of class `cpt.for`. If `Class=FALSE` a list with elements:
#' \itemize{
#'   \item{model: }{fitted ML model based upon training data}
#'   \item{errors: }{forecast errors}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' X = stats::arima.sim(list(ar=0.3), n=500)
#' trainData = stats::arima.sim(list(ar=0.3), n=100)
#' ans = forecastMlp(X, m=100, trainData=trainData)
#' summary(ans)
#' }
forecastMlp = function(X, m, trainData, trend=FALSE, Class=TRUE, ...){
  XX = X
  if(trend){
    df = data.frame('trainData'=trainData, 'Time'=1:length(trainData))
    trendModel = stats::lm(trainData~Time, data=df)
    trainData = trendModel$coefficients[1]-(1:length(trainData))*trendModel$coefficients[2]
    X = X-trendModel$coefficients[1]-((length(trainData)+1):(length(trainData)+length(X)))*trendModel$coefficients[2]
  }
  if(class(trainData)!="ts"){
    trainData = stats::ts(trainData)
  }
  if(class(X)!="ts"){
    X = stats::ts(X)
    XX = stats::ts(XX)
  }
  modelTrained = nnfor::mlp(trainData, ...)
  forecast = as.numeric(forecast::forecast(modelTrained,h=1)$mean)
  for(i in 2:length(X)){
    model = nnfor::mlp(stats::ts(c(trainData, X[1:(i-1)])), model = modelTrained)
    forecast[i] = as.numeric(forecast::forecast(model, h=1)$mean)
  }
  forecastErrors = X-forecast
  if(Class){
    return(new('cptFor',
               X=XX,
               m=m,
               model=modelTrained,
               errors=forecastErrors))
  }else{
    return(list('model'=model, 'errors'=forecastErrors))
  }
}
