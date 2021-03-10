#' Plot a list of `cptFor` objects
#'
#' @param x a list of `cptFor` objects
#' @param names a list of the names of the `cptFor` object being plotted
#'
#' @return ggplot
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plotMulti = function(x, names){
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
                              .data$X:.data$CUSUM2,
                              names_to='Series',
                              names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                              values_to='Value')
    tibThreshold = tibble::tibble('Method'=names[[1]],
                                  'Time'=1:(n-x[[1]]@m),
                                  'CUSUM'=x[[1]]@threshold,
                                  'CUSUM2'=x[[1]]@threshold2)
    tibThreshold = tidyr::pivot_longer(tibThreshold,
                                       .data$CUSUM:.data$CUSUM2,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                       values_to='Value')
    tibTau = tibble::tibble('Method'=names[[1]],
                            'CUSUM'=x[[1]]@tau,
                            'CUSUM2'=x[[1]]@tau2)
    tibTau = tidyr::pivot_longer(tibTau,
                                 !.data$Method,
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
                                    .data$X:.data$CUSUM2,
                                    names_to='Series',
                                    names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('X', 'Errors', 'CUSUM', 'CUSUM2'), ordered=TRUE)),
                                    values_to='Value')
      tib = dplyr::bind_rows(tib, tibTemp)
      tibThresholdTemp = tibble::tibble('Method'=names[[i]],
                                        'Time'=1:(n-x[[i]]@m),
                                        'CUSUM'=x[[i]]@threshold,
                                        'CUSUM2'=x[[i]]@threshold)
      tibThresholdTemp = tidyr::pivot_longer(tibThresholdTemp,
                                             .data$CUSUM:.data$CUSUM2,
                                             names_to='Series',
                                             names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                             values_to='Value')
      tibThreshold = dplyr::bind_rows(tibThreshold, tibThresholdTemp)

      tibTauTemp = tibble::tibble('Method'=names[[i]],
                                  'CUSUM'=x[[i]]@tau,
                                  'CUSUM2'=x[[i]]@tau2)
      tibTauTemp = tidyr::pivot_longer(tibTauTemp,
                                       !.data$Method,
                                       names_to='Series',
                                       names_transform = list(Series = ~ readr::parse_factor(.x, levels=c('CUSUM', 'CUSUM2'), ordered=TRUE)),
                                       values_to='Tau')
      tibTau = dplyr::bind_rows(tibTau, tibTauTemp)

      p = ggplot(tib, aes(x=.data$Time, y=.data$Value))+
        geom_line()+
        facet_grid(.data$Series~.data$Method, scales='free')+
        geom_line(data=tibThreshold, linetype='dashed', col='blue')
      addAnotate = purrr::map_lgl(x, ~ (.x@tau!=Inf)||(.x@tau2!=Inf))
      if(any(addAnotate)){
        p = p + geom_label(data=dplyr::filter(tibTau, .data$Tau!=Inf), aes(x=-Inf, y=Inf, label=paste0('tau=',.data$Tau)), hjust='left', vjust='top')
      }
    }
  }
  return(p)
}
