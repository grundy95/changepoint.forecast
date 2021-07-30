#' Simulate Critical Value
#'
#' Simulates a critical value from the asymptotic distribution of the chosen CUSUM detector under the no change scenario.
#' @param samples Number of samples to simulate from asymptotic distribution
#' @param alpha Type 1 error
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' @param gamma tuning parameter in the weight function
#' @param npts Number of points in discretization of Weiner process
#' @param progressBar Logical. Show progress bar
#'
#' @return numeric critical value
#' @export
#'
#' @examples
#' ans = simCritVal(samples=100, npts=20)
simCritVal = function(samples=1000, alpha=0.05, detector='PageCUSUM',
                      gamma=0, npts=500, progressBar=TRUE){
  limDistSamples = limDistGenerator(detector=detector, gamma=gamma,
                                    samples=samples, npts=npts,
                                    progressBar=progressBar)
  return(as.numeric(stats::quantile(limDistSamples,1-alpha)))
}

#' Samples from Limit Distributions of CUSUM Detectors
#'
#' Generates required number of samples from the asymptotic limit distribution of the chosen CUSUM detector under the scenario of no change.
#' @param detector character. Type of changepoint detector to use. Choice of
#' \itemize{
#'   \item{"PageCUSUM": }{Page's CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"PageCUSUM1": }{Page's CUSUM detector for 1-sided alternative hypothesis}
#'   \item{"CUSUM": }{Original CUSUM detector for 2-sided alternative hypothesis}
#'   \item{"CUSUM1": }{Original CUSUM detector for 1-sided alternative hypothesis}
#' }
#' @param gamma tuning parameter in the weight function
#' @param samples Number of samples to simulate from asymptotic distribution
#' @param npts Number of points in discretization of Weiner process
#' @param progressBar Logical. Show progress bar
#'
#' @return numeric vector of samples
limDistGenerator = function(detector='PageCUSUM', gamma=0, samples=1000, npts=500, progressBar=TRUE){
  weinerRaw = fdapace::Wiener(n=samples,pts=seq(0,1,length=npts), K=npts)
  if(detector=='PageCUSUM1'){
    if(progressBar){
      limSamples = pbapply::pbapply(weinerRaw, 1, p1, npts=npts, gamma=gamma)
    }else{
      limSamples = apply(weinerRaw, 1, p1, npts=npts, gamma=gamma)
    }
    return(limSamples)
  }else if(detector=='PageCUSUM'){
    if(progressBar){
      limSamples = pbapply::pbapply(weinerRaw, 1, p2, npts=npts, gamma=gamma)
    }else{
      limSamples = apply(weinerRaw, 1, p2, npts=npts, gamma=gamma)
    }
    return(limSamples)
  }else if(detector=='CUSUM1'){
    if(progressBar){
      limSamples = pbapply::pbapply(weinerRaw, 1, c1, npts=npts, gamma=gamma)
    }else{
      limSamples = apply(weinerRaw, 1, c1, npts=npts, gamma=gamma)
    }
    return(limSamples)
  }else if(detector=='CUSUM'){
    if(progressBar){
      limSamples = pbapply::pbapply(weinerRaw, 1, c2, npts=npts, gamma=gamma)
    }else{
      limSamples = apply(weinerRaw, 1, c2, npts=npts, gamma=gamma)
    }
    return(limSamples)
  }else{
    stop('Detector not recognized. Please choose from "PageCUSUM", PageCUSUM1", "CUSUM" or "CUSUM1"')
  }
}

#' Limit distribution: Original CUSUM, one-sided alternative
#'
#' @param w Sample Weiner process
#' @param npts Number of points in discretization of Weiner process
#' @param gamma tuning parameter
#'
#' @return numeric
c1 = function(w, npts, gamma){
  ans = purrr::map_dbl(1:npts, ~w[.x] / ((.x/npts)^gamma))
  max(ans)
}

#' Limit distribution: Original CUSUM, two-sided alternative
#'
#' @param w Sample Weiner process
#' @param npts Number of points in discretization of Weiner process
#' @param gamma tuning parameter
#'
#' @return numeric
c2 = function(w, npts, gamma){
  ans = purrr::map_dbl(1:npts, ~abs(w[.x]) / ((.x/npts)^gamma))
  max(ans)
}

#' Limit distribution: Page CUSUM, one-sided alternative
#'
#' @param w Sample Weiner process
#' @param npts Number of points in discretization of Weiner process
#' @param gamma tuning parameter
#'
#' @return numeric
p1 = function(w, npts, gamma){
  ans = purrr::map_dbl(1:(npts-1), ~(1 / ((.x/npts)^gamma)) * (w[.x] - p1inf(w, .x, npts)))
  return(max(ans))
}

#' Minimum of Weiner process in Page's one-sided CUSUM limit distribution
#'
#' @param w Sample Weiner process
#' @param t Max point to find minimum within
#' @param npts Number of points in discretization of Weiner process
#'
#' @return numeric
p1inf = function(w, t, npts){
  ans = purrr::map_dbl(1:t, ~((1-t/npts)/(1-.x/npts)) * w[.x])
  return(min(ans))
}

#' Limit distribution: Page's CUSUM, two-sided alternative
#'
#' @param w Sample Weiner process
#' @param npts Number of points in discretization of Weiner process
#' @param gamma tuning parameter
#'
#' @return numeric
p2 = function(w, npts, gamma){
  ans = purrr::map_dbl(1:(npts-1), ~p2sup(w=w, t=.x, npts=npts, gamma=gamma))
  return(max(ans))
}

#' Maximum of Weiner process in Page's two-sided CUSUM limit distribution
#'
#' @param w Sample Weiner process
#' @param t Max point to find minimum within
#' @param npts Number of points in discretization of Weiner process
#' @param gamma tuning parameter
#'
#' @return numeric
p2sup = function(w, t, npts, gamma){
  ans = purrr::map_dbl(1:t, ~1/((t/npts)^gamma) * abs(w[t] - ((1-t/npts)/(1-./npts)) * w[.]))
  return(max(ans))
}
