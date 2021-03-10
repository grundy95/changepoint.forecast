#' Sequential Changepoint Analysis of Forecast Errors
#'
#' This function performs sequential changepoint analysis on the supplied forecast errors
#' to detect when forecasts become inaccurate and need modifying.
#'
#' @param errors numeric vector or `cptFor` object. Forecast errors to perform changepoint analysis upon or `cptFor` object containing forecast errors and m
#' @param m numeric. Length of training period where forecast errors are assumed stable.
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' See details below
#' @param gamma numeric. Tuning parameter used in detector. See details below
#' @param critValue character or numeric. Critical value of normalized asymptotic distribution of chosen detector under no change. Choice of
#' \itemize{
#'   \item{"Lookup": }{Pre-simulated critical value is used if available}
#'   \item{"Simulate": }{Critical value is simulated. Note this can be time consuming in certain scenarios}
#'   \item{numeric: }{Specified critical value to be used}
#' }
#' See details below
#' @param alpha Type-1 error
#' @param samples Only used if `critValue='Simulate'`. Number of sample of Weiner processes used to calculate critical value.
#' @param npts  Only used if `critValue='Simulate'`. Number of points in each sample of a Weiner process used to calculate critical value.
#'
#' @return an object of class `cptFor`
#' @export
#'
#' @examples
#' forecastErrors = c(stats::rnorm(400), stats::rnorm(100,2))
#' ans = cptForecast(forecastErrors, m=300)
cptForecast = function(errors,
                       m=ceiling(length(errors)/2),
                       detector='PageCUSUM',
                       gamma=0,
                       critValue='Lookup',
                       alpha=0.05,
                       samples=1000,
                       npts=500){
  #errorCheck()
  if(class(errors)=="cptFor"){
    if(any(is.na(errors@errors))){
      stop("errors slot of S4 Class `cptFor` must contain vector of forecast errors")
    }
    if(is.na(errors@m)){
      errors@m = m
    }
    if(is.na(errors@detector)){
      errors@detector = detector
    }
    if(is.na(errors@gamma)){
      errors@gamma = gamma
    }
    if(is.na(errors@alpha)){
      errors@alpha = alpha
    }
    if(is.na(errors@errorsVar)){
      ans = cptSeqCUSUM(errors@errors, m=errors@m, detector=errors@detector, gamma=errors@gamma, sigma2=NULL, critValue=critValue, alpha=errors@alpha, samples=samples, npts=npts, Class=FALSE)
      errors@errorsVar = ans$sigma2
    }else{
      ans = cptSeqCUSUM(errors@errors, m=errors@m, detector=errors@detector, gamma=errors@gamma, sigma2=errors@errorsVar, critValue=critValue, alpha=errors@alpha, samples=samples, npts=npts, Class=FALSE)
    }
    errors@cusum = ans$cusum
    errors@threshold = ans$threshold
    errors@tau = ans$tau
    if(any(is.na(errors@errors2Var))){
      ans2 = cptSeqCUSUM(errors@errors^2, m=errors@m, detector=errors@detector, gamma=errors@gamma, sigma2=NULL, critValue=critValue, alpha=errors@alpha, samples=samples, npts=npts, Class=FALSE)
      errors@errors2Var = ans2$sigma2
    }else{
      ans2 = cptSeqCUSUM(errors@errors^2, m=errors@m, detector=errors@detector, gamma=errors@gamma, sigma2=errors@errors2Var, critValue=critValue, alpha=errors@alpha, samples=samples, npts=npts, Class=FALSE)
    }
    errors@cusum2 = ans2$cusum
    errors@threshold2 = ans2$threshold
    errors@tau2 = ans2$tau
    return(errors)
  }else{
    ans = cptSeqCUSUM(errors, m=m, detector=detector, gamma=gamma, sigma2=NULL, critValue=critValue, alpha=alpha, samples=samples, npts=npts, Class=FALSE)
    ans2 = cptSeqCUSUM(errors^2, m=m, detector=detector, gamma=gamma, sigma2=NULL, critValue=critValue, alpha=alpha, samples=samples, npts=npts, Class=FALSE)
    return(cptFor(errors=errors, m=m, gamma=gamma, detector=detector, alpha=alpha,
                  errorsVar=ans$sigma2, errors2Var=ans2$sigma2,
                  cusum=ans$cusum, cusum2=ans2$cusum,
                  threshold=ans$threshold, threshold2=ans2$threshold,
                  tau=ans$tau, tau2=ans2$tau))
  }
}
