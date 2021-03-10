library(tidyverse)
critValueTableCreator <- function(gamma, alpha, samples=1000, npts=500){
  tib = list()
  count = 0
  pb = txtProgressBar(min=0, max=length(gamma)*length(alpha),style=3)
  for(i in 1:length(gamma)){
    for(j in 1:length(alpha)){
      critValCusum = simCritVal(samples=samples, alpha=alpha[j], detector='CUSUM', gamma=gamma[i], npts=npts, progressBar=FALSE)
      critValCusum1sided = simCritVal(samples=samples, alpha=alpha[j], detector='CUSUM1', gamma=gamma[i], npts=npts, progressBar=FALSE)
      critValPCusum = simCritVal(samples=samples, alpha=alpha[j], detector='PageCUSUM', gamma=gamma[i], npts=npts, progressBar=FALSE)
      critValPCusum1sided = simCritVal(samples=samples, alpha=alpha[j], detector='PageCUSUM1', gamma=gamma[i], npts=npts, progressBar=FALSE)

      count = count+1
      tib[[count]] = tibble::tibble('Detector'=c('CUSUM', 'CUSUM1', 'PageCUSUM', 'PageCUSUM1'), 'Gamma'=gamma[i], 'Alpha'=alpha[j], 'CritVal'=c(critValCusum, critValCusum1sided, critValPCusum, critValPCusum1sided))
      setTxtProgressBar(pb, count)
    }
  }
  close(pb)
  tib = dplyr::bind_rows(tib)
  return(tib)
}

gamma = seq(0,0.45,by=0.025)
alpha = c(0.01,0.05,0.1)
critValTable = critValueTableCreator(gamma,alpha)

usethis::use_data(critValTable, overwrite = TRUE, internal=TRUE)
