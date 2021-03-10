setOldClass("ts", "numeric")
setClassUnion("tsORnumeric", c("ts", "numeric"))
#' S4 class for a forecast changepoint object
#'
#' List of the basic method
#'
#' @slot errors ts or numeric vector. Forecast errors, one-step-ahead
#' @slot m numeric. Length of training data
#' @slot X ts or numeric vector. Raw time series
#' @slot model ANY. Summary of model used for forecasting
#' @slot detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' @slot gamma numeric. Tuning parameter within detector
#' @slot errorsVar numeric. Variance of forecast errors in training data
#' @slot errors2Var numeric. Variance of squares of forecast errors
#' @slot cusum numeric. CUSUM values of forecast errors
#' @slot cusum2 numeric. CUSUM values of squares of forecast errors
#' @slot alpha numeric. Type=1 error
#' @slot threshold numeric. threshold values for CUSUM values of forecast errors
#' @slot threshold2 numeric. threshold values for CUSUM values of squares of forecast errors
#' @slot tau numeric. Time point when changepoint is flagged based upon forecast errors
#' @slot tau2 numeric. Time point when changepoint is flagged based upon squares of forecast errors
#'
#' @return An object of class `cptFor`.
#'
#' @import methods
#' @export
cptFor = setClass("cptFor",
                  representation(errors="tsORnumeric",
                                 m="numeric",
                                 X="tsORnumeric",
                                 model="ANY",
                                 detector="character",
                                 gamma="numeric",
                                 errorsVar="numeric",
                                 errors2Var="numeric",
                                 cusum="numeric",
                                 cusum2="numeric",
                                 alpha="numeric",
                                 threshold="numeric",
                                 threshold2="numeric",
                                 tau="numeric",
                                 tau2="numeric"),
                  prototype(errors=NA_real_,
                            m=NA_real_,
                            X=NA_real_,
                            model=NULL,
                            detector=NA_character_,
                            gamma=NA_real_,
                            errorsVar=NA_real_,
                            errors2Var=NA_real_,
                            cusum=NA_real_,
                            cusum2=NA_real_,
                            alpha=NA_real_,
                            threshold=NA_real_,
                            threshold2= NA_real_,
                            tau = NA_real_,
                            tau2 = NA_real_)
)

#' @describeIn cptFor Plot of `cptFor` object
#'
#' @param x an object of class `cptFor`
#'
#' @aliases plot,cptFor-method
#' @import ggplot2
#' @importFrom rlang .data
#' @export
setMethod("plot", "cptFor", function(x){
  n = length(x@errors)
  if(any(is.na(x@cusum2))){
    if(any(is.na(x@X))){
      tib = tibble::tibble('Time'=1:(n-x@m),
                           'Errors'=x@errors[(x@m+1):n],
                           'CUSUM'=x@cusum)
      tib = tidyr::pivot_longer(tib,
                                !.data$Time,
                                names_to='Series',
                                names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('Errors', 'CUSUM'), ordered=TRUE)),
                                values_to='Value')
    }else{
      tib = tibble::tibble('Time'=1:(n-x@m),
                           'X'=x@X[(x@m+1):n],
                           'Errors'=x@errors[(x@m+1):n],
                           'CUSUM'=x@cusum)
      tib = tidyr::pivot_longer(tib,
                                !.data$Time,
                                names_to='Series',
                                names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM'), ordered=TRUE)),
                                values_to='Value')
    }
    tibThreshold = tibble::tibble('Time'=1:(n-x@m),
                                  'CUSUM'=x@threshold)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       !.data$Time,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM'), ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('CUSUM'=x@tau)
    tibTau = tidyr::pivot_longer(tibTau,
                                 tidyselect::everything(),
                                 names_to='Series',
                                 names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM'), ordered=TRUE)),
                                 values_to='Tau')
  }else{
    if(any(is.na(x@X))){
      tib = tibble::tibble('Time'=1:(n-x@m),
                           'Errors'=x@errors[(x@m+1):n],
                           'CUSUM'=x@cusum,
                           'CUSUM2'=x@cusum2)
      tib = tidyr::pivot_longer(tib,
                                !.data$Time,
                                names_to='Series',
                                names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                                values_to='Value')
    }else{
      tib = tibble::tibble('Time'=1:(n-x@m),
                           'X'=x@X[(x@m+1):n],
                           'Errors'=x@errors[(x@m+1):n],
                           'CUSUM'=x@cusum,
                           'CUSUM2'=x@cusum2)
      tib = tidyr::pivot_longer(tib,
                                !.data$Time,
                                names_to='Series',
                                names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                                values_to='Value')
    }
    tibThreshold = tibble::tibble('Time'=1:(n-x@m),
                                  'CUSUM'=x@threshold,
                                  'CUSUM2'=x@threshold2)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       !.data$Time,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('CUSUM'=x@tau,
                            'CUSUM2'=x@tau2)
    tibTau = tidyr::pivot_longer(tibTau,
                                 tidyselect::everything(),
                                 names_to='Series',
                                 names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                 values_to='Tau')
  }

  p = ggplot(tib, aes(x=.data$Time, y=.data$Value))+
    geom_line()+
    facet_grid(.data$Series~., scales='free')+
    geom_line(data=tibThreshold, linetype='dashed', col='blue')
  if((x@tau<Inf)||(!is.na(x@tau2) && x@tau2<Inf)){
    p = p + geom_label(data=dplyr::filter(tibTau, .data$Tau!=Inf), aes(x=-Inf, y=Inf, label=paste0('tau=',.data$Tau)), hjust='left', vjust='top')
  }
  return(p)
})





