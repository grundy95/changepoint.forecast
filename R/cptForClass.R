#' S4 class for a forecast changepoint object
#'
#' List of the basic method
#'
#' @slot errors numeric vector. Forecast errors, one-step-ahead
#' @slot m numeric. Length of training data
#' @slot detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' @slot forecastErrorType character. Type of changes to look for. Choice of
#' \itemize{
#'   \item{"Both": }{Analysis is performed on both the raw and squared forecast errors,}
#'   \item{"Raw": }{Analysis is only performed on the raw forecast errors. Only first
#'   order changes are reliably detected.}
#'   \item{"Squared:" }{Analysis is only performed on the centred squared forecast errors.}
#' }
#' @slot gamma numeric. Tuning parameter within detector
#' @slot errorsVar numeric. Variance of forecast errors in training data
#' @slot errors2Var numeric. Variance of squares of forecast errors
#' @slot cusum numeric. CUSUM values of forecast errors
#' @slot cusum2 numeric. CUSUM values of squares of forecast errors
#' @slot alpha numeric. Type-1 error
#' @slot critValue numeric. Critical value used in the threshold to maintain type-1 error rate
#' @slot threshold numeric. threshold values for CUSUM values of forecast errors
#' @slot threshold2 numeric. threshold values for CUSUM values of squares of forecast errors
#' @slot tau numeric. Time point when changepoint is flagged based upon forecast errors
#' @slot tau2 numeric. Time point when changepoint is flagged based upon squares of forecast errors
#' @slot updateStats numeric vector. Update statistics needed for use with `updateForecast`
#' @slot updateStats2 numeric vector. Update statistics needed for use with `updateForecast`
#' @slot version character. Version of changepoint.forecast version
#' @slot date character. Date object was created/updated
#'
#' @return An object of class `cptFor`.
#'
#' @import methods
#' @export
cptFor = setClass("cptFor",
                  representation(errors="numeric",
                                 m="numeric",
                                 detector="character",
                                 forecastErrorType="character",
                                 gamma="numeric",
                                 errorsVar="numeric",
                                 errors2Var="numeric",
                                 cusum="numeric",
                                 cusum2="numeric",
                                 alpha="numeric",
                                 critValue="numeric",
                                 threshold="numeric",
                                 threshold2="numeric",
                                 tau="numeric",
                                 tau2="numeric",
                                 updateStats="numeric",
                                 updateStats2="numeric",
                                 version="character",
                                 date="character"),
                  prototype(errors=NA_real_,
                            m=NA_real_,
                            detector=NA_character_,
                            forecastErrorType='Raw',
                            gamma=NA_real_,
                            errorsVar=NA_real_,
                            errors2Var=NA_real_,
                            cusum=NA_real_,
                            cusum2=NA_real_,
                            alpha=NA_real_,
                            critValue=NA_real_,
                            threshold=NA_real_,
                            threshold2= NA_real_,
                            tau = NA_real_,
                            tau2 = NA_real_,
                            updateStats = NA_real_,
                            updateStats2 = NA_real_,
                            version=as(packageVersion("changepoint.forecast"),'character'),
                            date=date())
)
#' cptFor Methods
#'
#' Methods for objects with S4 class \code{\linkS4class{cptFor}}
#'
#' @param object An object of S4 class \code{\linkS4class{cptFor}}
#'
#' @name cptFor-methods
#'
NULL

#' @describeIn cptFor-methods Summary of `cptFor` object
#'
#' @export
setMethod("summary", "cptFor", function(object){
  cat('Created using changepoint.forecast version',object@version, 'on', object@date,'\n')
  cat('Detector: ',
      object@detector,'\n')
  cat('Changepoint Detected: ',
      (!is.na(object@tau)&&object@tau<Inf)||(!is.na(object@tau2)&&object@tau2<Inf),'\n')
  if((!is.na(object@tau)&&object@tau<Inf)||(!is.na(object@tau2)&&object@tau2<Inf)==TRUE){
    if((!is.na(object@tau)&&object@tau<Inf)&&(!is.na(object@tau2)&&object@tau2<Inf)){
      if(object@tau==object@tau2){
        cat('Changepoint Detected in: Raw and Squared Forecast Errors \n')
        cat('Changepoint Detected at: ', object@tau, '\n')
      }else if(object@tau<object@tau2){
        cat('Changepoint Detected in: Raw Forecast Errors \n')
        cat('Changepoint Detected at: ', object@tau, '\n')
      }else{
        cat('Changepoint Detected in: Squared Forecast Errors \n')
        cat('Changepoint Detected at: ', object@tau2, '\n')
      }
    }else if(!is.na(object@tau)&&object@tau<Inf){
      cat('Changepoint Detected in: Raw Forecast Errors \n')
      cat('Changepoint Detected at: ', object@tau, '\n')
    }else{
      cat('Changepoint Detected in: Squared Forecast Errors \n')
      cat('Changepoint Detected at: ', object@tau2, '\n')
    }
  }
})

#' @describeIn cptFor-methods Show `cptFor` object
#'
#' @export
setMethod('show','cptFor',function(object){
  cat('Class, cpt.for	: Forecast Changepoint Object\n')
  cat('			: S4 class containing', length(attributes(object))-1,'slots with names\n')
  cat('			 ',names(attributes(object))[1:(length(attributes(object))-1)],'\n\n')
  cat('Created on 	:', object@date,'\n\n')
  cat('Summary(.)	:\n----------\n')
  summary(object)
})

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
  if(any(is.na(x@cusum))&&any(is.na(x@cusum2))){
    stop("cusum or cusum2 slots needs to be filled to plot")
  }else if(any(is.na(x@cusum2))){
    tib = tibble::tibble('Time'=1:(n-x@m),
                         'Errors'=x@errors[(x@m+1):n],
                         'CUSUM'=x@cusum)
    tib = tidyr::pivot_longer(tib,
                              !.data$Time,
                              names_to='Series',
                              names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                    levels=c('Errors',
                                                                                             'CUSUM'),
                                                                                    ordered=TRUE)),
                              values_to='Value')
    tibThreshold = tibble::tibble('Time'=1:(n-x@m),
                                  'CUSUM'=x@threshold)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       !.data$Time,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                             levels=c('CUSUM'),
                                                                                             ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('CUSUM'=x@tau)
    tibTau = tidyr::pivot_longer(tibTau,
                                 tidyselect::everything(),
                                 names_to='Series',
                                 names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                       levels=c('CUSUM'),
                                                                                       ordered=TRUE)),
                                 values_to='Tau')
  }else if(any(is.na(x@cusum))){
    tib = tibble::tibble('Time'=1:(n-x@m),
                         'Errors'=x@errors[(x@m+1):n],
                         'CUSUM2'=x@cusum2)
    tib = tidyr::pivot_longer(tib,
                              !.data$Time,
                              names_to='Series',
                              names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                    levels=c('Errors',
                                                                                             'CUSUM2'),
                                                                                    ordered=TRUE)),
                              values_to='Value')
    tibThreshold = tibble::tibble('Time'=1:(n-x@m),
                                  'CUSUM2'=x@threshold2)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       !.data$Time,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                             levels=c('CUSUM2'),
                                                                                             ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('CUSUM2'=x@tau2)
    tibTau = tidyr::pivot_longer(tibTau,
                                 tidyselect::everything(),
                                 names_to='Series',
                                 names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                       levels=c('CUSUM2'),
                                                                                       ordered=TRUE)),
                                 values_to='Tau')
  }else{
    tib = tibble::tibble('Time'=1:(n-x@m),
                         'Errors'=x@errors[(x@m+1):n],
                         'CUSUM'=x@cusum,
                         'CUSUM2'=x@cusum2)
    tib = tidyr::pivot_longer(tib,
                              !.data$Time,
                              names_to='Series',
                              names_transform = list(Series = ~ readr::parse_factor(.x,
                                                                                    levels=c('Errors',
                                                                                             'CUSUM',
                                                                                             'CUSUM2'),
                                                                                    ordered=TRUE)),
                              values_to='Value')
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
  if((!is.na(x@tau)&&x@tau<Inf)||(!is.na(x@tau2)&&x@tau2<Inf)){
    p = p + geom_label(data=dplyr::filter(tibTau, .data$Tau!=Inf), aes(x=-Inf, y=Inf, label=paste0('tau=',.data$Tau)), hjust='left', vjust='top')
  }
  return(p)
})

#' Retrieval Functions - Generic
#'
#' @param x object of class \code{\linkS4class{cptFor}}
#' @name retrievalGeneric
NULL

#' Replacement Functions - Generic
#'
#' @param x object of class \code{\linkS4class{cptFor}}
#' @param value value
#' @name replacementGeneric
NULL

#' Retrieval Functions - Method
#'
#' @param x object of class \code{\linkS4class{cptFor}}
#' @name retrievalMethod
NULL

#' Replacement Functions - Method
#'
#' @param x object of class \code{\linkS4class{cptFor}}
#' @param value value
#' @name replacementMethod
NULL

#' @rdname retrievalGeneric
#' @export
setGeneric("errors", function(x){
  standardGeneric("errors")
})

#' @rdname replacementGeneric
#' @export
setGeneric("errors<-", function(x, value){
  standardGeneric("errors<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("errors", "cptFor", function(x){
  x@errors
})

#' @rdname replacementMethod
#' @export
setMethod("errors<-", "cptFor", function(x, value){
  x@errors <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("m", function(x){
  standardGeneric("m")
})

#' @rdname replacementGeneric
#' @export
setGeneric("m<-", function(x, value){
  standardGeneric("m<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("m", "cptFor", function(x){
  x@m
})

#' @rdname replacementMethod
#' @export
setMethod("m<-", "cptFor", function(x, value){
  x@m <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("detector", function(x){
  standardGeneric("detector")
})

#' @rdname replacementGeneric
#' @export
setGeneric("detector<-", function(x, value){
  standardGeneric("detector<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("detector", "cptFor", function(x){
  x@detector
})

#' @rdname replacementMethod
#' @export
setMethod("detector<-", "cptFor", function(x, value){
  x@detector <- value
})


#' @rdname replacementGeneric
#' @export
setGeneric("gamma<-", function(x, value){
  standardGeneric("gamma<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("gamma", "cptFor", function(x){
  x@gamma
})

#' @rdname replacementMethod
#' @export
setMethod("gamma<-", "cptFor", function(x, value){
  x@gamma <- value
})
#' @rdname retrievalGeneric
#' @export
setGeneric("errorsVar", function(x){
  standardGeneric("errorsVar")
})

#' @rdname replacementGeneric
#' @export
setGeneric("errorsVar<-", function(x, value){
  standardGeneric("errorsVar<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("errorsVar", "cptFor", function(x){
  x@errorsVar
})

#' @rdname replacementMethod
#' @export
setMethod("errorsVar<-", "cptFor", function(x, value){
  x@errorsVar <- value
})
#' @rdname retrievalGeneric
#' @export
setGeneric("errors2Var", function(x){
  standardGeneric("errors2Var")
})

#' @rdname replacementGeneric
#' @export
setGeneric("errors2Var<-", function(x, value){
  standardGeneric("errors2Var<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("errors2Var", "cptFor", function(x){
  x@errors2Var
})

#' @rdname replacementMethod
#' @export
setMethod("errors2Var<-", "cptFor", function(x, value){
  x@errors2Var <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("cusum", function(x){
  standardGeneric("cusum")
})

#' @rdname replacementGeneric
#' @export
setGeneric("cusum<-", function(x, value){
  standardGeneric("cusum<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("cusum", "cptFor", function(x){
  x@cusum
})

#' @rdname replacementMethod
#' @export
setMethod("cusum<-", "cptFor", function(x, value){
  x@cusum <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("cusum2", function(x){
  standardGeneric("cusum2")
})

#' @rdname replacementGeneric
#' @export
setGeneric("cusum2<-", function(x, value){
  standardGeneric("cusum2<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("cusum2", "cptFor", function(x){
  x@cusum2
})

#' @rdname replacementMethod
#' @export
setMethod("cusum2<-", "cptFor", function(x, value){
  x@cusum2 <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("alpha", function(x){
  standardGeneric("alpha")
})

#' @rdname replacementGeneric
#' @export
setGeneric("alpha<-", function(x, value){
  standardGeneric("alpha<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("alpha", "cptFor", function(x){
  x@alpha
})

#' @rdname replacementMethod
#' @export
setMethod("alpha<-", "cptFor", function(x, value){
  x@alpha <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("critValue", function(x){
  standardGeneric("critValue")
})

#' @rdname replacementGeneric
#' @export
setGeneric("critValue<-", function(x, value){
  standardGeneric("critValue<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("critValue", "cptFor", function(x){
  x@critValue
})

#' @rdname replacementMethod
#' @export
setMethod("critValue<-", "cptFor", function(x, value){
  x@critValue <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("threshold", function(x){
  standardGeneric("threshold")
})

#' @rdname replacementGeneric
#' @export
setGeneric("threshold<-", function(x, value){
  standardGeneric("threshold<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("threshold", "cptFor", function(x){
  x@threshold
})

#' @rdname replacementMethod
#' @export
setMethod("threshold<-", "cptFor", function(x, value){
  x@threshold <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("threshold2", function(x){
  standardGeneric("threshold2")
})

#' @rdname replacementGeneric
#' @export
setGeneric("threshold2<-", function(x, value){
  standardGeneric("threshold2<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("threshold2", "cptFor", function(x){
  x@threshold2
})

#' @rdname replacementMethod
#' @export
setMethod("threshold2<-", "cptFor", function(x, value){
  x@threshold2 <- value
})
#' @rdname retrievalGeneric
#' @export
setGeneric("tau", function(x){
  standardGeneric("tau")
})

#' @rdname replacementGeneric
#' @export
setGeneric("tau<-", function(x, value){
  standardGeneric("tau<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("tau", "cptFor", function(x){
  x@tau
})

#' @rdname replacementMethod
#' @export
setMethod("tau<-", "cptFor", function(x, value){
  x@tau <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("tau2", function(x){
  standardGeneric("tau2")
})

#' @rdname replacementGeneric
#' @export
setGeneric("tau2<-", function(x, value){
  standardGeneric("tau2<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("tau2", "cptFor", function(x){
  x@tau2
})

#' @rdname replacementMethod
#' @export
setMethod("tau2<-", "cptFor", function(x, value){
  x@tau2 <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("updateStats", function(x){
  standardGeneric("updateStats")
})

#' @rdname replacementGeneric
#' @export
setGeneric("updateStats<-", function(x, value){
  standardGeneric("updateStats<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("updateStats", "cptFor", function(x){
  x@updateStats
})

#' @rdname replacementMethod
#' @export
setMethod("updateStats<-", "cptFor", function(x, value){
  x@updateStats <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("forecastErrorType", function(x){
  standardGeneric("forecastErrorType")
})

#' @rdname replacementGeneric
#' @export
setGeneric("forecastErrorType<-", function(x, value){
  standardGeneric("forecastErrorType<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("forecastErrorType", "cptFor", function(x){
  x@forecastErrorType
})

#' @rdname replacementMethod
#' @export
setMethod("forecastErrorType<-", "cptFor", function(x, value){
  x@forecastErrorType <- value
})

#' @rdname retrievalGeneric
#' @export
setGeneric("updateStats2", function(x){
  standardGeneric("updateStats2")
})

#' @rdname replacementGeneric
#' @export
setGeneric("updateStats2<-", function(x, value){
  standardGeneric("updateStats2<-")
})

#' @rdname retrievalMethod
#' @export
setMethod("updateStats2", "cptFor", function(x){
  x@updateStats2
})

#' @rdname replacementMethod
#' @export
setMethod("updateStats2<-", "cptFor", function(x, value){
  x@updateStats2 <- value
})
