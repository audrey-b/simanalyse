#' Summarises the results of simulation studies.
#'
#' Summarises the results of simulation studies. 
#' The results are compared with the true parameter values by calculating Monte Carlo measures 
#' such as bias, mean square error and coverage probability.
#' R code can be used to customize the summary measures and to coerce results of analyses to
#' nlist when neccessary.
#' 
#' @param results.nlists An nlists of results
#' @param measures A vector of strings indicating which Monte Carlo measures to calculate. Strings may include "bias", "E" (expectation), 
#' "cp.quantile" (coverage probability of quantile-based CrIs), "cp.length" (coverage probability of quantile-based CrIs),
#' "Epvar" (expected posterior variance), "Epsd" (expected posterior standard deviation)
#' @param parameters Parameters to use to calculate Monte Carlo measures such as bias and coverage probability
#' @param estimator A function, typically mean or median, for the Bayes estimator to use.
#' @param alpha scalar representing the alpha level used to construct credible intervals. Default is 0.05.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the summary. By default all stochastic nodes are included.
#' @param custom_expr A string of R code to define custom measures; estimator and parameters are reserved keywords. E.g. "bias = estimator - parameters".
#' @return A flag.
#' @export
#'
#' @examples
#' params <- nlist(mu=0)
#' dat <- sims::sims_simulate("a ~ dnorm(mu, 1)", 
#'                            parameters = params, 
#'                            nsims=5)
#' result <- sma_analyse_bayesian(datalist=dat,
#'                                       code = "a ~ dnorm(mu, 1)
#'                                               mu ~ dunif(-3,3)",
#'                                       n.adapt = 101,
#'                                       n.burnin = 0,
#'                                       n.iter = 101,
#'                                       monitor="mu")
#' sma_summarise(result, parameters=params)

#  custom_expr A string of R functions to define custom measures.
#  custom_derive
#  parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#  path A string specifying the path to the directory where the results were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#  exists A flag specifying whether the summaries should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#  silent A flag specifying whether to suppress warnings.
# @param derive A vector of strings indicating which Monte Carlo measures to derive from \code{measures}. Strings may include "rb" (relative 
# bias), "br" (bias ratio), "var" (variance), "se" (standard error), rmse (root mean square error), "rrmse" (relative root mean square error)

sma_summarise <- function(results.nlists, 
                          measures=c("bias", "mse", "cp.quantile"), 
                          estimator=mean, 
                          alpha=0.05,
                          parameters,
                          monitor=".*",
                          custom_expr){
        
        check_list(results.nlists) #lapply checks needs to be added
        check_character(measures)
        check_function(estimator)
        check_nlist(parameters)
        check_chr(monitor)
        check_scalar(alpha) #how can I print error is not between zero and 1?
        
        if(!missing(custom_expr)){check_string(custom_expr)}
        
        if(monitor != ".*") results.nlists %<>% lapply(subset, select=monitor)
        
        summarise_all_measures(results.nlists, 
                               measures, 
                               parameters,
                               estimator,
                               alpha) %>% return
}


