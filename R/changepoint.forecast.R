#' changepoint.forecast: An R package aimed at monitoring forecast errors for detecting changepoints
#'
#' The changepoint.forecast package monitors provided forecast errors for changepoints in first and second
#' order changes. These include the common mean and variance changes. The main function is
#' \code{\link{cptForecast}} which looks for changepoints in a set of forecast errors and then the function
#' \code{\link{updateForecast}} can be used to analyse new forecast errors from the same forecasting model
#' without repeating the initial analysis. This allows for the online implementation of the method as new
#' forecast errors become available.
#'
#' @section Main functions:
#' \code{\link{cptForecast}}: Perform sequential changepoint analysis on forecast errors.
#'
#' \code{\link{updateForecast}}: Update sequential changepoint analysis on new forecast errors.
#'
#' \code{\link{cptSeqCUSUM}}: Perform generic CUSUM based sequential changepoint analysis on data.
#'
#' @section cptFor S4 class:
#' The main functions return a \code{\linkS4class{cptFor}} object by default. This S4 class object can
#' then be passed to \code{\link{updateForecast}} along with new forecast errors to update the analysis.
#' The methods `show`, `summary` and `plot` can be used on the \code{\linkS4class{cptFor}} object to
#' see the main results of the analysis. Furthermore the slots of \code{\linkS4class{cptFor}} can be
#' accessed using the `@` (this works similarily to the `$` for S3 objects). Moreover, a set of
#' retriever functions are available for example to access the errors slots of an object `ans`, you would
#' call `errors(ans)`.
#'
#' @references
#' \insertRef{Grundy2021}{changepoint.forecast}
#'
#' \insertRef{Fremdt2014}{changepoint.forecast}
#'
#' @docType package
#' @name changepoint.forecast
NULL
