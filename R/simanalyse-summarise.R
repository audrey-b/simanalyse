#' Summarises the results of simulation studies.
#'
#' Summarises the results of simulation studies. 
#' The results are compared with the true parameter values by calculating Monte Carlo measures 
#' such as bias, mean square error and coverage probability.
#' R code can be used to customize the summary measures and to coerce results of analyses to
#' nlist when neccessary.
#' 
#' @param results.nlists An nlists of results
#' @param measures A vector of strings indicating which Monte Carlo measures to calculate. Strings may include "bias", "rb" (relative 
#' bias), "br" (bias ratio), "var" (variance), "se" (standard error), "mse" (mean square error), rmse (root mean square 
#' error), "rrmse" (relative root mean square error), "cp90", "cp95" and "cp99" (coverage probability of 90, 95 and 99 percent quantile-based CrIs),
#' "Epvar" (expected posterior variance), "Epsd" (expected posterior standard deviation)
#' @param estimator A function, typically mean or median, for the Bayes estimator to use.
#' @param parameters Parameters to use to calculate Monte Carlo measures such as bias and coverage probability
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the summary. By default all stochastic nodes are included.
#' 
#' @return A flag.
#' @export
#'

#need to add example
#  code A string of R functions to define custom measures.
#  parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#  path A string specifying the path to the directory where the results were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#  exists A flag specifying whether the summaries should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#  silent A flag specifying whether to suppress warnings.

simanalyse_summarise <- function(results.nlists, 
                                 measures, 
                                 estimator=mean, 
                                 parameters,
                                 monitor=".*"){
        
        check_list(results.nlists) #lapply checks needs to be added
        check_chr(measures)
        check_function(estimator)
        check_nlist(parameters)
        check_chr(monitor)
        
        if(monitor != ".*") results.nlists %<>% lapply(subset, select=monitor)
        
        summarise_one_measure(results.nlists, 
                              measures, 
                              estimator,
                              parameters) %>% return
        
        
}


