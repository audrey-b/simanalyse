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
#' "Epvar" (expected posterior variance), "Epsd" (expected posterior standard deviation), "rb" (relative 
#'  bias), "br" (bias ratio), "var" (variance), "se" (standard error), rmse (root mean square error), "rrmse" (relative root mean square error),
#' "cv" (coefficient of variation), "all" (all the measures)
#' @param parameters Parameters to use to calculate Monte Carlo measures such as bias and coverage probability
#' @param estimator A function, typically mean or median, for the Bayes estimator to use.
#' @param alpha scalar representing the alpha level used to construct credible intervals. Default is 0.05.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the summary. By default all stochastic nodes are included.
#' @param custom_FUNS A string of R code to define functions over the mcmc samples which will be passed to \code{custom_expr_before}. E.g. list(posteriormedian = median).
#' @param custom_expr_before A string of R code to derive custom measures. This code is used BEFORE averaging over all simulations. E.g. "mse = (posteriormedian - parameters)^2". Functions from \code{custom_FUNS} may be used as well as the keywords 'parameters' (the true values of the parameters) and 'estimator' (the estimator defined in \code{estimator}).
#' @param custom_expr_after A string of R code to derive additional custom measures. This code is used AFTER averaging over all simulations. E.g. "rmse = sqrt(mse)". Measures calculated from \code{custom_expr_before} may be used as well as the keyword 'parameters' (the true values of the parameters). 
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
#' sma_summarise(result, 
#' measures="", 
#' parameters = params, 
#' custom_FUNS = list(estimator = mean),
#' custom_expr_before = "bias = estimator - parameters")


#  thin
#  parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#  path A string specifying the path to the directory where the results were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#  exists A flag specifying whether the summaries should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#  silent A flag specifying whether to suppress warnings.

sma_summarise <- function(results.nlists, 
                          measures=c("bias", "mse", "cp.quantile"), 
                          estimator=mean, 
                          alpha=0.05,
                          parameters,
                          monitor=".*",
                          custom_FUNS,
                          custom_expr_before,
                          custom_expr_after=NULL){
        
        check_list(results.nlists) #lapply checks needs to be added
        check_character(measures)
        check_function(estimator)
        check_nlist(parameters)
        check_chr(monitor)
        check_scalar(alpha) #how can I print error is not between zero and 1?
        
        if(!missing(custom_expr_before)){check_string(custom_expr_before)} else custom_expr_before=""
        if(!missing(custom_expr_after)){check_string(custom_expr_after)} else custom_expr_after=""
        if(!missing(custom_FUNS)){
                check_list(custom_FUNS)
                lapply(custom_FUNS, check_function)
        }else custom_FUNS=NULL
        
        if(monitor != ".*") results.nlists %<>% lapply(subset, select=monitor)
        
        summarise_all_measures(results.nlists, 
                               make_expr_and_FUNS(measures, parameters, estimator, alpha, custom_FUNS, custom_expr_before, custom_expr_after), 
                               parameters) %>% return
}


