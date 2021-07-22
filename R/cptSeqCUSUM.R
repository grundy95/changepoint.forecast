#' Sequential Changepoint Analysis via a CUSUM Detector
#'
#' This function performs sequential changepoint analysis using a chosen CUSUM detector.
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
  #errorCheck()
  N = length(X)
  n = N-m

  if(detector=='PageCUSUM'){
    cusumFun = cusumPageGenerator(X=X, m=m, oneSidedAlt=FALSE)
  }else if(detector=='PageCUSUM1'){
    cusumFun = cusumPageGenerator(X=X, m=m, oneSidedAlt=TRUE)
  }else if(detector=='CUSUM'){
    cusumFun = cusumGenerator(X=X, m=m, oneSidedAlt=FALSE)
  }else if(detector=='CUSUM1'){
    cusumFun = cusumGenerator(X=X, m=m, oneSidedAlt=TRUE)
  }else{
    stop('changepoint detector not supported.
         Please choose between "PageCUSUM", "PageCUSUM1", "CUSUM" or "CUSUM1"')
  }
  cusumValues = purrr::map_dbl(1:n, cusumFun)
  weightValues = purrr::map_dbl(1:n, ~weightFun(m=m, k=.x, gamma=gamma))
  if(critValue=='Lookup'){
    critValue = critValLookup(gamma=gamma, alpha=alpha, detector=detector)
  }else if(critValue=='Simulate'){
    critValue = simCritVal(samples=samples, alpha=alpha, detector=detector, gamma=gamma, npts=npts)
  }else if(!is.numeric(critValue)){
    stop('critValue should either be "Lookup", "Simulate" or a single number containing the critical value to be used')
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
               errorsVar = sigma2,
               cusum = cusumValues,
               threshold = thresholdValues,
               tau = tau))
  }else{
    return(list('tau'=tau,
                'cusum'=cusumValues,
                'threshold'=thresholdValues,
                'sigma2' = sigma2))
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
