#' An S4 class for a forecast changepoint object
#'
#' @slot X ANY. Raw time series
#' @slot m numeric. Length of training data
#' @slot model ANY. Summary of model used for forecasting
#' @slot errors ANY. Forecast errors, one-step-ahead
#' @slot errorsVar numeric. Variance of forecast errors in training data
#' @slot errors2Var numeric. Variance of squares of forecast errors
#' @slot cusum numeric. CUSUM values of forecast errors
#' @slot cusum2 numeric. CUSUM values of squares of forecast errors
#' @slot cusumThreshold numeric. threshold values for CUSUM values of forecast errors
#' @slot cusum2Threshold numeric. threshold values for CUSUM values of squares of forecast errors
#' @slot tau numeric. Time point when changepoint is flagged based upon forecast errors
#' @slot tau2 numeric. Time point when changepoint is flagged based upon squares of forecast errors
#'
#' @return An object of class `cptFor`.
#'
#' @import methods
#' @export
#'
#' @examples
setClass("cptFor",
         representation(X="ANY",
                        m="numeric",
                        model="ANY",
                        errors="ANY",
                        errorsVar="numeric",
                        errors2Var="numeric",
                        cusum="numeric",
                        cusum2="numeric",
                        cusumThreshold="numeric",
                        cusum2Threshold="numeric",
                        tau="numeric",
                        tau2="numeric"),
         prototype(X=NA,
                   m=NA_real_,
                   model=NA,
                   errors=NA,
                   errorsVar=NA_real_,
                   errors2Var=NA_real_,
                   cusum=NA_real_,
                   cusum2=NA_real_,
                   cusumThreshold=NA_real_,
                   cusum2Threshold= NA_real_,
                   tau = NA_real_,
                   tau2 = NA_real_)
)

if(!isGeneric("calcCusum")){
  if(is.function("calcCusum")){
    fun = calcCusum
  }else{
    fun = function(x, ...){
      standardGeneric("calcCusum")
    }
  }
  setGeneric("calcCusum", fun)
}
#' Sequential changepoint analysis of Forecast Errors
#'
#' Takes an object of class `cptFor` and (re)caclulates the CUSUM of the forecast errors and squared forecast errors.
#'
#' @param cptFor An object of `cptFor` with the following slots filled:
#' \itemize{
#'   \item{errors: }{forecast errors must be provided}
#'   \item{m: }{length of training period must be provided}
#' }
#' @param gamma tuning parameter used in CUSUM detector
#'
#'
#' @return an object of class `cptFor`
#' @export
#'
#' @examples
#' obj = new("cptFor", errors=stats::rnorm(500), m=300)
#' obj = calCusum(obj)
#' summary(obj)
#' plot(obj)
setMethod("calcCusum", "cptFor", function(x, gamma=0, rootDir='./'){
  if(is.na(x@errorsVar)){
    ans = cptSeqCUSUM(x@errors, m=x@m, gamma=gamma)
    x@errorsVar = ans$sigma2
  }else{
    ans = cptSeqCUSUM(x@errors, m=x@m, gamma=gamma, sigma2=x@errorsVar)
  }
  x@cusum = ans$cusum
  x@cusumThreshold = ans$threshold
  x@tau = ans$tau
  if(is.na(x@errors2Var)){
    ans2 = cptSeqCUSUM(x@errors^2, m=x@m, gamma=gamma)
    x@errors2Var = ans2$sigma2
  }else{
    ans2 = cptSeqCUSUM(x@errors^2, m=x@m, gamma=gamma, sigma2=x@errors2Var)
  }
  x@cusum2 = ans2$cusum
  x@cusum2Threshold = ans2$threshold
  x@tau2 = ans2$tau
  return(x)
})


#' Plot of `cptFor` object
#'
#' Produces a ggplot containg the raw data, forecast errors, CUSUM of forecast errors and squared forecast errors along with thresholds.
#'
#' @param cptFor an object of class `cptFor`
#'
#' @return ggplot
#'
#' @import ggplot2
#' @export
#'
#' @examples
setMethod("plot", "cptFor", function(x){
  n = length(x@X)
  tib = tibble::tibble('Time'=1:(n-x@m),
                       'X'=x@X[(x@m+1):n],
                       'Errors'=x@errors[(x@m+1):n],
                       'CUSUM'=x@cusum,
                       'CUSUM2'=x@cusum2)
  tib = tidyr::pivot_longer(tib,
                            !Time,
                            names_to='Series',
                            names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                            values_to='Value')
  tibThreshold = tibble::tibble('Time'=1:(n-x@m),
                                'CUSUM'=x@cusumThreshold,
                                'CUSUM2'=x@cusum2Threshold)
  tibThreshold = tidyr::pivot_longer(tibThreshold,
                                     !Time,
                                     names_to='Series',
                                     names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                     values_to='Value')
  tibTau = tibble::tibble('CUSUM'=x@tau,
                          'CUSUM2'=x@tau2)
  tibTau = tidyr::pivot_longer(tibTau,
                               everything(),
                               names_to='Series',
                               names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                               values_to='Tau')

  p = ggplot(tib, aes(x=Time, y=Value))+
    geom_line()+
    facet_grid(Series~., scales='free')+
    geom_line(data=tibThreshold, linetype='dashed', col='blue')
  if((x@tau<Inf)||(x@tau2<Inf)){
    p = p + geom_label(data=dplyr::filter(tibTau, Tau!=Inf), aes(x=-Inf, y=Inf, label=paste0('tau=',Tau)), hjust='left', vjust='top')
  }
  return(p)
})

if(!isGeneric("plotMulti")){
  if(is.function("plotMulti")){
    fun = plotMulti
  }else{
    fun = function(x, ...){
      standardGeneric("plotMulti")
    }
  }
  setGeneric("plotMulti", fun)
}

#' Plot of list of `cptFor` objects
#'
#' Produces a ggplot where each column is the plot of each `cptFor` object in the list.
#'
#' @param x a list of `cptFor` objects
#'
#' @return ggplot
#'
#' @import ggplot2
#' @export
#'
#' @examples
setMethod("plotMulti", "list", function(x, names){
  if(length(x)==1){
    p = plot(x[[1]])+labs(title=names[[1]])
  }else{
    n = length(x[[1]]@X)
    tib = tibble::tibble('Method'=names[[1]],
                         'Time'=1:(n-x[[1]]@m),
                         'X'=x[[1]]@X[(x[[1]]@m+1):n],
                         'Errors'=x[[1]]@errors[(x[[1]]@m+1):n],
                         'CUSUM'=x[[1]]@cusum,
                         'CUSUM2'=x[[1]]@cusum2)
    tib = tidyr::pivot_longer(tib,
                              X:CUSUM2,
                              names_to='Series',
                              names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                              values_to='Value')
    tibThreshold = tibble::tibble('Method'=names[[1]],
                                  'Time'=1:(n-x[[1]]@m),
                                  'CUSUM'=x[[1]]@cusumThreshold,
                                  'CUSUM2'=x[[1]]@cusum2Threshold)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       CUSUM:CUSUM2,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('Method'=names[[1]],
                            'CUSUM'=x[[1]]@tau,
                            'CUSUM2'=x[[1]]@tau2)
    tibTau = tidyr::pivot_longer(tibTau,
                                 !Method,
                                 names_to='Series',
                                 names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                 values_to='Tau')
    for(i in 2:length(x)){
      tibTemp = tibble::tibble('Method'=names[[i]],
                                'Time'=1:(n-x[[i]]@m),
                                'X'=x[[i]]@X[(x[[i]]@m+1):n],
                                'Errors'=x[[i]]@errors[(x[[i]]@m+1):n],
                                'CUSUM'=x[[i]]@cusum,
                                'CUSUM2'=x[[i]]@cusum2)
      tibTemp = tidyr::pivot_longer(tibTemp,
                                    X:CUSUM2,
                                    names_to='Series',
                                    names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                                    values_to='Value')
      tib = dplyr::bind_rows(tib, tibTemp)
      tibThresholdTemp = tibble::tibble('Method'=names[[i]],
                                        'Time'=1:(n-x[[i]]@m),
                                        'CUSUM'=x[[i]]@cusumThreshold,
                                        'CUSUM2'=x[[i]]@cusum2Threshold)
      tibThresholdTemp = tidyr::pivot_longer(tibThresholdTemp,
                                             CUSUM:CUSUM2,
                                             names_to='Series',
                                             names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                             values_to='Value')
      tibThreshold = dplyr::bind_rows(tibThreshold, tibThresholdTemp)

      tibTauTemp = tibble::tibble('Method'=names[[i]],
                                  'CUSUM'=x[[i]]@tau,
                                  'CUSUM2'=x[[i]]@tau2)
      tibTauTemp = tidyr::pivot_longer(tibTauTemp,
                                       !Method,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                       values_to='Tau')
      tibTau = dplyr::bind_rows(tibTau, tibTauTemp)

      p = ggplot(tib, aes(x=Time, y=Value))+
        geom_line()+
        facet_grid(Series~Method, scales='free')+
        geom_line(data=tibThreshold, linetype='dashed', col='blue')
      addAnotate = map_lgl(x, ~ (.x@tau!=Inf)||(.x@tau2!=Inf))
      if(any(addAnotate)){
        p = p + geom_label(data=dplyr::filter(tibTau, Tau!=Inf), aes(x=-Inf, y=Inf, label=paste0('tau=',Tau)), hjust='left', vjust='top')
      }
    }
  }
  return(p)
})
