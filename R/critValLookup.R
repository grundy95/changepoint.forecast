#' Lookup Critical Value
#'
#' Lookup pre-simulated critical value of asymptotic distribution of chosen CUSUM detector
#' @param gamma tuning parameter
#' @param alpha type 1 error
#' @param CUSUMtype character. Choice between 'Page' and 'Original' CUSUM detector. See details below
#' @param oneSidedAlt Logical. Use of the one-sided or two-sided CUSUM detectors
#'
#' @return
#' @export
#'
#' @examples
#' critValLookup(gamma=0,
#'               alpha=0.05,
#'               CUSUMtype='Page',
#'               oneSidedAlt=FALSE)
critValLookup = function(gamma=0, alpha=0.05, CUSUMtype='Page', oneSidedAlt=FALSE){
  CUSUMtype=toupper(CUSUMtype)
  if(!(gamma %in% critValTable$Gamma)){
    stop(paste0(c('gamma value not stored in table. Either simulate a critical value using function simCritVal or try one of the following gamma values:\n',unique(critValTable$Gamma)), collapse=' '))
  }
  if(!(alpha %in% critValTable$Alpha)){
    stop(paste0(c('alpha value not stored in table. Either simulate a critical value using function simCritVal or try one of the following alpha values:\n',unique(critValTable$Alpha)), collapse=' '))
  }
  if(!((CUSUMtype=='PAGE')||(CUSUMtype=='ORIGINAL'))){
    stop('CUSUMtype not found. Please use "Page" or "Original"')
  }
  if(!is.logical(oneSidedAlt)){
    stop('oneSidedAlt should be logical. If TRUE then the critical value for a one-sided CUSUM is returned. If FALSE the two-sided critical value is returned')
  }

  if(oneSidedAlt){
    H1 = 'One-sided'
  }else{
    H1 = 'Two-sided'
  }
  ans = dplyr::filter(critValTable, CusumType==CUSUMtype, Alternative==H1, Gamma==gamma, Alpha==alpha)
  return(ans$CritVal)
}
