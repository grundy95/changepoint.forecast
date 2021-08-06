#' Update Forecast Errors
#'
#' This function updates a `cptFor` object with new forecast errors. This allows for
#' the online implementation of the method as new forecast errors become available.
#'
#' @param newErrors numeric vector. New Forecast errors to perform changepoint analysis upon..
#' @param model `cptFor` object. Output of call to function `cptForecast` where initial forecast error
#' were analysed.
#'
#' @return An object of class `cptFor`
#' @export


updateForecast = function(newErrors, model){
  #checks on model and newErrors
  cusumLen = length(model@cusum)
  trainMean = model@updateStats[1]
  newWeights = purrr::map_dbl((cusumLen+1):(cusumLen+length(newErrors)),
                              ~weightFun(m=model@m, k=.x, gamma=model@gamma))

  if((model@forecastErrorType=='Raw')||(model@forecastErrorType=='Both')){
    newCusum = model@cusum[cusumLen]
    if(model@detector=='CUSUM1'){
      for(i in 1:length(newErrors)){
        newCusum[i+1] = newCusum[i] + newErrors[i] - trainMean
      }
    }else if(model@detector=='CUSUM'){
      cusumA = model@updateStats[2]
      for(i in 1:length(newErrors)){
        cusumA = cusumA + newErrors[i] - trainMean
        newCusum[i+1] = abs(cusumA)
      }
      model@updateStats[2] = c(cusumA)
    }else if(model@detector=='PageCUSUM1'){
      for(i in 1:length(newErrors)){
        newCusum[i+1] = max(newCusum[i] + newErrors[i] - trainMean, 0)
      }
    }else{
      cusumA = model@updateStats[2]
      cusumB = model@updateStats[3]
      for(i in 1:length(newErrors)){
        cusumA = max(cusumA + newErrors[i] - trainMean, 0)
        cusumB = max(cusumB - newErrors[i] + trainMean, 0)
        newCusum[i+1] = max(cusumA, cusumB)
      }
      model@updateStats[2:3] = c(cusumA, cusumB)
    }
    newCusum = newCusum[-1]
    newThresholdValues = newWeights*model@critValue*sqrt(model@errorsVar)
    thresholdExceeded = newCusum>newThresholdValues
    if(all(!thresholdExceeded)){
      tau = Inf
    }else{
      tau = cusumLen+min(which(thresholdExceeded==TRUE))
    }

    model@cusum = c(model@cusum, newCusum)
    model@threshold = c(model@threshold, newThresholdValues)
    model@tau = min(model@tau, tau)

  }
  if((model@forecastErrorType=='Sqaured')||(model@forecastErrorType=='Both')){
    newErrors2 = (newErrors-trainMean)^2
    trainMean2 = model@updateStats2[1]
    newCusum2 = model@cusum2[cusumLen]
    if(model@detector=='CUSUM1'){
      for(i in 1:length(newErrors2)){
        newCusum2[i+1] = newCusum2[i] + newErrors2[i] - trainMean2
      }
    }else if(model@detector=='CUSUM'){
      cusumA = model@updateStats2[2]
      for(i in 1:length(newErrors2)){
        cusumA = cusumA + newErrors2[i] - trainMean2
        newCusum2[i+1] = abs(cusumA)
      }
      model@updateStats2[2] = cusumA
    }else if(model@detector=='PageCUSUM1'){
      for(i in 1:length(newErrors2)){
        newCusum2[i+1] = max(newCusum2[i] + newErrors2[i] - trainMean2, 0)
      }
    }else{
      cusumA = model@updateStats2[2]
      cusumB = model@updateStats2[3]
      for(i in 1:length(newErrors2)){
        cusumA = max(cusumA + newErrors2[i] - trainMean2, 0)
        cusumB = max(cusumB - newErrors2[i] + trainMean2, 0)
        newCusum2[i+1] = max(cusumA, cusumB)
      }
      model@updateStats2[2:3] = c(cusumA, cusumB)
    }
    newCusum2 = newCusum2[-1]
    newThresholdValues2 = newWeights*model@critValue*sqrt(model@errors2Var)
    thresholdExceeded2 = newCusum2>newThresholdValues2
    if(all(!thresholdExceeded2)){
      tau2 = Inf
    }else{
      tau2 = cusumLen+min(which(thresholdExceeded2==TRUE))
    }

    model@cusum2 = c(model@cusum2, newCusum2)
    model@threshold2 = c(model@threshold2, newThresholdValues2)
    model@tau2 = min(model@tau2, tau2)
  }
  model@errors = c(model@errors, newErrors)
  return(model)
}


