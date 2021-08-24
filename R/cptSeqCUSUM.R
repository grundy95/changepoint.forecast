#' Sequential Changepoint Analysis via a CUSUM Detector
#'
#' This function performs sequential changepoint analysis using a chosen CUSUM detector.
#'
#' For more details on all the majority of arguments see \code{\link{cptForecast}}. Note here X
#' is equivalent to errors in \code{\link{cptForecast}}. This function is identical to calling
#' \code{\link{cptForecast}} with `forecastErrorType="Raw"`.
#'
#' @param X time series. Can be a numeric vector or a `ts` object
#' @param m length of training period
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' @param gamma tuning parameter in the weight function. See details below
#' @param sigma2 variance of data in training period. Default is `NULL` and variance is estimated using `var()` (assumes independence).
#' @param critValue numeric or character:
#' \itemize{
#'   \item{numeric: }{This will be used as the critical value. Note this is not the raw threshold.}
#'   \item{'Lookup': }{a pre-simulated critical value is used if available.}
#'   \item{'Simulate': }{a critical value is simulated, dependent on scenario this can be time consuming.}
#' }
#' @param alpha asymptotic type 1 error rate. Not used if critValue is a numeric.
#' @param samples Only used if `critValue='Simulate'`. Number of sample of Weiner processes used to calculate critical value.
#' @param npts  Only used if `critValue='Simulate'`. Number of points in each sample of a Weiner process used to calculate critical value.
#' @param Class Logical. Return an object of class `cptFor`.
#'
#' @return an object of class `cptFor`.
#' If `Class=FALSE` a list with
#' \itemize{
#'   \item tau: Time a change is flagged
#'   \item cusum: Vector of cusum values
#'   \item threshold: Vector of threshold values
#'   \item sigma2: Variance of points in training period
#' }
#' @export
#'
#' @examples
#' X = c(stats::rnorm(400), stats::rnorm(100, 2))
#' ans = cptSeqCUSUM(X, m=300)
#' summary(ans)
#' plot(ans)
cptSeqCUSUM = function(X,
                       m = ceiling(0.5*length(X)),
                       detector='PageCUSUM',
                       gamma=0,
                       sigma2=NULL,
                       critValue = 'Lookup',
                       alpha=0.05,
                       samples=1000,
                       npts=500,
                       Class = TRUE){
  cptSeqCUSUMerroCheck(X=X, m=m, detector=detector, gamma=gamma, sigma2=sigma2,
                       critValue=critValue, alpha=alpha, samples=samples,
                       npts=npts, Class=Class)
  N = length(X)
  n = N-m

  cusumValues = 0
  cusumA = 0
  cusumB = 0
  trainMean = mean(X[1:m])
  if(detector=='PageCUSUM'){
    for(i in 1:n){
      cusumA = max(cusumA + X[i+m] - trainMean, 0)
      cusumB = max(cusumB - X[i+m] + trainMean, 0)
      cusumValues[i+1] = max(cusumA, cusumB)
    }
  }else if(detector=='PageCUSUM1'){
    for(i in 1:n){
      cusumValues[i+1] = max(cusumValues[i] + X[m+i] - trainMean, 0)
    }
  }else if(detector=='CUSUM'){
    for(i in 1:n){
      cusumA = cusumA + X[i+m] - trainMean
      cusumValues[i+1] = abs(cusumA)
    }
  }else{
    for(i in 1:n){
      cusumValues[i+1] = cusumValues[i] + X[m+i] - trainMean
    }
  }

  cusumValues = cusumValues[-1]
  weightValues = purrr::map_dbl(1:n, ~weightFun(m=m, k=.x, gamma=gamma))
  if(critValue=='Lookup'){
    critValue = critValLookup(gamma=gamma, alpha=alpha, detector=detector)
  }else if(critValue=='Simulate'){
    critValue = simCritVal(samples=samples, alpha=alpha, detector=detector, gamma=gamma, npts=npts)
  }else if(!is.numeric(critValue)){
    stop("critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  }
  if(is.null(sigma2)){
    sigma2 = stats::var(X[1:m])
  }
  thresholdValues = weightValues*critValue*sqrt(sigma2)
  thresholdExceeded = cusumValues>thresholdValues
  if(all(!thresholdExceeded)){
    tau = Inf
  }else{
    tau = min(which(thresholdExceeded==TRUE))
  }
  if(Class){
    return(new("cptFor",
               errors = X,
               m = m,
               gamma=gamma,
               critValue=critValue,
               detector = detector,
               errorsVar = sigma2,
               cusum = cusumValues,
               threshold = thresholdValues,
               tau = tau,
               updateStats = c(trainMean, cusumA, cusumB)))
  }else{
    return(list('tau'=tau,
                'cusum'=cusumValues,
                'threshold'=thresholdValues,
                'sigma2' = sigma2,
                'critValue' = critValue,
                'updateStats' = c(trainMean, cusumA, cusumB)))
  }
}

#' CUSUM Weight Function
#'
#' @param m Length of training period
#' @param k Time location
#' @param gamma tuning parameter
#'
#' @return numeric weight
weightFun = function(m, k, gamma=0){
  if(!((gamma>=0)&&(gamma<0.5))){
    stop('gamma must be between 0 and 1/2')
  }
  return(sqrt(m)*(1 + k/m)*((k/(k+m))^gamma))
}

#' Error checking - cptSeqCUSUM
#'
#' Performs error checking on arguments given to cptSeqCUSUM
#'
#' @inheritParams cptSeqCUSUM
cptSeqCUSUMerroCheck= function(X=X, m=m, detector=detector, gamma=gamma, sigma2=sigma2,
                     critValue=critValue, alpha=alpha, samples=samples,
                     npts=npts, Class=Class){
  ## X
  if(any(!is.numeric(X))){
    stop("X should be a vector of numeric values with no NA values")
  }else if(any(is.na(X))||any(X==Inf)){
    stop("X should be a vector of numeric values with no NA values")
  }

  ## m
  if(length(m)!=1 || is.na(m)){
    stop("m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  }else if(m != as.integer(m)){
    stop("m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  }else if((m<2)||(m>=length(X))){
    stop("m should be a single positive integer such that 1<m<n, where n is the length of
         the vector of errors")
  }

  ## detector
  if(!(detector %in% c("CUSUM", "CUSUM1", "PageCUSUM", "PageCUSUM1"))){
    stop("Detector must be one of 'CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'. Note these
    are case-sensitive")
  }

  ## gamma
  if(any(!is.numeric(gamma))||any(is.na(gamma))){
    stop("gamma should be a single positive number such that 0<=gamma<0.5")
  }else if(length(gamma)!=1){
    stop("gamma should be a single positive number such that 0<=gamma<0.5")
  }else if(!((gamma>=0)&&(gamma<0.5))){
    stop("gamma should be a single positive number such that 0<=gamma<0.5")
  }

  ## sigma2
  if(any(!is.null(sigma2))){
    if(any(!is.numeric(sigma2))||any(is.na(sigma2))){
      stop("sigma2 should be a single positive number greater than 0")
    }else if(length(sigma2)!=1){
      stop("sigma2 should be a single positive number greater than 0")
    }else if(!((sigma2>=0)&&(sigma2<Inf))){
      stop("sigma2 should be a single positive number greater than 0")
    }
  }
  ## critValue
  if(any(is.numeric(critValue))){
    if(length(critValue)!=1){
      stop("critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
    }else if((critValue<0)||(critValue==Inf)||is.na(critValue)){
      stop("critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
    }
  }else if(!(critValue %in% c("Lookup", "Simulate"))){
    stop("critValue should either be 'Lookup', 'Simulate' or a single positive numeric")
  }

  ## alpha
  if(any(!is.numeric(alpha))||any(is.na(alpha))){
    stop("alpha should be a single positive number such that 0<=alpha<=1")
  }else if(length(alpha)!=1){
    stop("alpha should be a single positive number such that 0<=alpha<=1")
  }else if((alpha<0)||(alpha>1)){
    stop("alpha should be a single positive number such that 0<=alpha<=1")
  }

  ## samples
  if(any(!is.numeric(samples))||any(is.na(samples))){
    stop("samples should be a single positive integer greater than 20")
  }else if(length(samples)!=1){
    stop("samples should be a single positive integer greater than 20")
  }else if(samples==Inf || samples != as.integer(samples)){
    stop("samples should be a single positive integer greater than 20")
  }else if(samples<20){
    stop("samples should be a single positive integer greater than 20")
  }

  ## npts
  if(any(!is.numeric(npts))||any(is.na(npts))){
    stop("npts should be a single positive integer greater than 20")
  }else if(length(npts)!=1){
    stop("npts should be a single positive integer greater than 20")
  }else if(npts==Inf || npts != as.integer(npts)){
    stop("npts should be a single positive integer greater than 20")
  }else if(npts<20){
    stop("npts should be a single positive integer greater than 20")
  }

  ## Class
  if(!(Class %in% c(TRUE, FALSE))){
    stop("Class should be logical, TRUE or FALSE")
  }
}
