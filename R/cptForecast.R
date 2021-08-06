#' Sequential Changepoint Analysis of Forecast Errors
#'
#' This function performs sequential changepoint analysis on supplied forecast errors
#' to detect when forecasts become inaccurate and need modifying.
#'
#' @param errors numeric vector. Forecast errors to perform changepoint analysis upon.
#' @param m numeric. Length of training period where forecast errors are assumed stable.
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis.}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis.}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis.}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis.}
#' }
#' See details below.
#' @param forecastErrorType character. Type of changes to look for. Choice of
#' \itemize{
#'   \item{"Both": }{Analysis is performed on both the raw and squared forecast errors,}
#'   \item{"Raw": }{Analysis is only performed on the raw forecast errors. Only first
#'   order changes are reliably detected.}
#'   \item{"Squared:" }{Analysis is only performed on the centred squared forecast errors.}
#' }
#' See details below.
#' @param gamma numeric. Tuning parameter used in detector. See details below.
#' @param critValue character or numeric. Critical value of normalized asymptotic distribution
#' of chosen detector under no change. Choice of
#' \itemize{
#'   \item{"Lookup": }{Pre-simulated critical value is used if available}
#'   \item{"Simulate": }{Critical value is simulated. Note this can be time consuming
#'   in certain scenarios}
#'   \item{numeric: }{Specified critical value to be used}
#' }
#' See details below
#' @param alpha Type-1 error
#' @param samples Only used if `critValue='Simulate'`. Number of sample of Weiner processes
#' used to calculate critical value.
#' @param npts  Only used if `critValue='Simulate'`. Number of points in each sample of a
#' Weiner process used to calculate critical value.
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
                       forecastErrorType='Both',
                       gamma=0,
                       critValue='Lookup',
                       alpha=0.05,
                       samples=1000,
                       npts=500){
  #errorCheck()
  if(forecastErrorType=='Both'){
    ans = cptSeqCUSUM(errors, m=m, detector=detector, gamma=gamma, sigma2=NULL,
                      critValue=critValue, alpha=alpha, samples=samples,
                      npts=npts, Class=FALSE)
    ans2 = cptSeqCUSUM((errors-mean(errors[1:m]))^2, m=m, detector=detector, gamma=gamma,
                       sigma2=NULL, critValue=critValue, alpha=alpha, samples=samples,
                       npts=npts, Class=FALSE)
    return(cptFor(errors=errors, m=m, gamma=gamma, detector=detector,
                  forecastErrorType=forecastErrorType,
                  alpha=alpha, critValue=ans$critValue,
                  errorsVar=ans$sigma2, errors2Var=ans2$sigma2,
                  cusum=ans$cusum, cusum2=ans2$cusum,
                  threshold=ans$threshold, threshold2=ans2$threshold,
                  tau=ans$tau, tau2=ans2$tau,
                  updateStats=ans$updateStats, updateStats2=ans2$updateStats))
  }else if(forecastErrorType=='Raw'){
    ans = cptSeqCUSUM(errors, m=m, detector=detector, gamma=gamma, sigma2=NULL,
                      critValue=critValue, alpha=alpha, samples=samples,
                      npts=npts, Class=FALSE)
    return(cptFor(errors=errors, m=m, gamma=gamma, detector=detector, alpha=alpha,
                  forecastErrorType=forecastErrorType,
                  critValue = ans$critValue,
                  errorsVar=ans$sigma2, cusum=ans$cusum,
                  threshold=ans$threshold, tau=ans$tau, updateStats=ans$updateStats))
  }else if(forecastErrorType=='Squared'){
    ans2 = cptSeqCUSUM((errors-mean(errors[1:m]))^2, m=m, detector=detector, gamma=gamma,
                       sigma2=NULL, critValue=critValue, alpha=alpha, samples=samples,
                       npts=npts, Class=FALSE)
    return(cptFor(errors=errors, m=m, gamma=gamma, detector=detector, alpha=alpha,
                  forecastErrorType=forecastErrorType,
                  critValue=ans2$critValue,
                  errors2Var=ans2$sigma2, cusum2=ans2$cusum,
                  threshold2=ans2$threshold, tau2=ans2$tau,
                  updateStats=c(mean(errors[1:m]), 0, 0), updateStats2=ans2$updateStats))
  }else{
    stop('forecastErrorType not supported. Please use one of "Both", "Raw" or "Squared"')
  }
}



