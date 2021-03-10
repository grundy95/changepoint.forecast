#' Lookup Critical Value
#'
#' Lookup pre-simulated critical value of asymptotic distribution of chosen CUSUM detector
#' @param gamma tuning parameter
#' @param alpha type 1 error
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#'
#' @return numeric.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' critValLookup(gamma=0,
#'               alpha=0.05,
#'               detector='PageCUSUM')
critValLookup = function(gamma=0, alpha=0.05, detector='PageCUSUM'){
  if(!(gamma %in% critValTable$Gamma)){
    stop(paste0(c('gamma value not stored in table. Either simulate a critical value using function simCritVal or try one of the following gamma values:\n',unique(critValTable$Gamma)), collapse=' '))
  }
  if(!(alpha %in% critValTable$Alpha)){
    stop(paste0(c('alpha value not stored in table. Either simulate a critical value using function simCritVal or try one of the following alpha values:\n',unique(critValTable$Alpha)), collapse=' '))
  }
  if(!(detector %in% critValTable$Detector)){
    stop(paste0(c('detector not in table. Either simulate a critical value using function simCritVal or try one of the following detectors:\n',unique(critValTable$Detector)), collapse=' '))
  }
  ans = dplyr::filter(critValTable, .data$Detector==detector, .data$Gamma==gamma, .data$Alpha==alpha)
  return(ans$CritVal)
}
