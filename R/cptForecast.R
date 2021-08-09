#' Sequential Changepoint Analysis of Forecast Errors
#'
#' Performs sequential changepoint analysis on supplied forecast errors
#' to detect when forecasts become inaccurate and need modifying.
#'
#' This function is designed to work alongside a forecasting model to automatically detect if/when
#' the forecasting model becomes inaccurate. By monitoring the forecast errors, this function
#' can automatically detect when a changepoint has occurred in the forecast errors indicating that the
#' forecasting model being used needs updating. This function can detect first-order changes (e.g.
#' mean changes) and second-order changes (e.g. variance changes) using the different `forecastErrorTypes`.
#' See below for more details. Once analysis has been performed using this method then
#' the \code{\link{updateForecast}} function can be used to analyse new forecast errors from the same
#' forecasting model with repeating the analysis that has already been performed.
#'
#' The function takes a numeric vector of forecast errors of size `n`. The first `m` time points (as
#' chosen by the user) determines the training period where no changepoints are deemed to have occurred.
#' Monitoring of the forecast errors begins from time point `m+1` and the returned
#' \code{\linkS4class{cptFor}} class contains a wide range of information including the time point
#' a changepoint was detected. This is the time points after monitoring has begun not from the start of
#' numeric vector of errors.
#'
#' There is a choice of four different detectors to use. In general, we suggest using `detector="PageCUSUM"`
#' unless you are specifically looking for a mean/variance increase in the forecast errors in which case
#' `detector="PageCUSUM1"`is recommended. We only recommend the `CUSUM` detectors if you are confident
#' the changepoint will happen shortly after monitoring has begun. For more information on these detectors
#' and the hyperparameter `gamma` see \insertCite{Fremdt2014;textual}{changepoint.forecast}.
#'
#' The choice of `forecastErrorType` depends on the type of change you expect to see in the data. This
#' defaults to `forecastErrorType="Both"` which performs analysis on the raw forecast errors (designed for
#' detecting mean changes) and the squared forecast errors (designed for detecting variance changes). If
#' you are only interested in mean (first-order) changes then you can chose to only use the raw forecast
#' errors with `forecastErrorType="Raw"` or similarly for detecting mean and/or variance changes you can
#' just use the squared forecast errors with `forecastErrorType="Squared"`. For more information, see
#' \insertCite{Grundy2021;textual}{changepoint.forecast}.
#'
#'  The crucial parameter for obtaining fast detection of changes depends upon the `critValue` which
#'  affects the threshold the CUSUM statistic must exceed to detect a change. A number of theoretical
#'  critical values are stored in look-up tables which can be accessed using `crtiVale="Lookup"`. This
#'  depends on the choice of `detector`, `gamma` and `alpha`, where `alpha` is the probability of a
#'  false alarm as `m` gets large. If the critical value for your chosen parameters is not
#'  available then this can be simulated using `critValue="Simulate"`. Here you can choose some additional
#'  hyperparameters (`samples` and `npts`) which alter how the critical value is simulated. Note for
#'  larger values of `samples` and `npts` dependent on the `detector` this can take a substantial
#'  amount of time to run. Finally, a numeric can be entered for `critValue` i.e. from a previous call to
#'  \code{\link{simCritVal}}. Note this numeric is not the threshold used but a part of it. For more
#'  details on the full threshold used see \insertCite{Grundy2021;textual}{changepoint.forecast}.
#'
#'  Finally, see \code{\linkS4class{cptFor}} for more information on the resulting S4 class object and
#'  how to use this to get relevant information from the analysis. Note the use of `@` rather than `$`
#'  to access slots.
#'
#'
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
#'
#' @importFrom Rdpack reprompt
#' @export
#'
#' @references
#' \insertRef{Fremdt2014}{changepoint.forecast}
#'
#' \insertRef{Grundy2021}{changepoint.forecast}
#'
#' @examples
#' # Mean change in forecast errors
#' forecastErrors = c(stats::rnorm(400), stats::rnorm(100,2))
#' ans = cptForecast(forecastErrors, m=300)
#' summary(ans)
#' plot(ans)
#'
#' # Variance change in forecast errors
#' forecastErrors2 = c(stats::rnorm(400), stats::rnorm(100,0,3))
#' ans2 = cptForecast(forecastErrors, m=300, forecastErrorType="Squared")
#' show(ans)
#' plot(ans)
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



