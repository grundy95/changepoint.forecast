#' Sequential Changepoint Analysis via a CUSUM Detector
#'
#' This function performs sequential changepoint analysis using a chosen CUSUM detector.
#'
#' @param X time series. Can be a numeric vector or a `ts` object
#' @param m length of training period
#' @param sigma2 variance of data in training period. Default is `NULL` and variance is estimated using `var()` (assumes indpendence).
#' @param gamma tuning parameter in the weight function. See details below
#' @param CUSUMtype character. Choice between 'Page' and 'Original' CUSUM detector. See details below
#' @param oneSidedAlt Logical. Use of the one-sided or two-sided CUSUM detectors
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
                       m = 0.2*length(X),
                       sigma2=NULL,
                       gamma=0,
                       CUSUMtype='Page',
                       oneSidedAlt=FALSE,
                       critValue = 'Lookup',
                       alpha=0.05,
                       samples=1000,
                       npts=500,
                       Class = TRUE){
  CUSUMtype = toupper(CUSUMtype)
  if(is.character(critValue)){
    critValue = toupper(critValue)
  }
  if(!is.ts(X)){
    X = ts(X)
  }

  N = length(X)
  n = N-m

  if(CUSUMtype=='PAGE'){
    cusumFun = cusumPageGenerator(X=X, m=m, oneSidedAlt=oneSidedAlt)
  }else if(CUSUMtype=='ORIGINAL'){
    cusumFun = cusumGenerator(X=X, m=m, oneSidedAlt=oneSidedAlt)
  }else{
    stop('CUSUMtype not supported. Please choose between "Page" and Original"')
  }
  cusumValues = purrr::map_dbl(1:n, cusumFun)

  weightValues = purrr::map_dbl(1:n, ~weightFun(m=m, k=.x, gamma=gamma))
  if(critValue=='LOOKUP'){
    critValue = critValLookup(gamma=gamma, alpha=alpha, CUSUMtype=CUSUMtype, oneSidedAlt=oneSidedAlt, rootDir=rootDir)
  }else if(critValue=='SIMULATE'){
    critValue = simCritVal(samples=samples, alpha=alpha, CUSUMtype=CUSUMtype, oneSidedAlt=oneSidedAlt, gamma=gamma, npts=npts)
  }else if(!is.numeric(critValue)){
    stop('critValue should either be "Lookup", "Simulate" or a single number containing the critical value to be used')
  }

  if(is.null(sigma2)){
    sigma2 = var(X[1:m])
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
               m = m,
               errors = X,
               errorsVar = sigma2,
               cusum = cusumValues,
               cusumThreshold = thresholdValues,
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
