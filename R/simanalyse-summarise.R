#' Summarises the results of simulation studies.
#'
#' Summarises the results of simulation studies. 
#' The results are compared with the true parameter values by calculating Monte Carlo measures 
#' such as bias, mean square error and coverage probability.
#' R code can be used to customize the summary measures and to coerce results of analyses to
#' nlist when neccessary.
#' 
#' @param measures A vector of strings indicating which Monte Carlo measures to calculate. Strings may include
#' "bias", "rb" (relative bias), "br" (bias ratio), "var" (variance), "se" (standard error),
#' "mse" (mean square error), "rmse" (root mean square error), "rrmse" (relative root mean square error), 
#' "cp" (coverage probability)
#' @param code A string of R functions to define custom measures.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the summary. By default all stochastic nodes are included.
#' @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#' @param path A string specifying the path to the directory where the results were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#' @param exists A flag specifying whether the summaries should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#' @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'
#' @examples
#' simanalyse_summarise()
#' simanalyse_summarise(FALSE)
# simanalyse_summarise <- function(x = TRUE) {
#   check_flag(x)
#   x
# }

