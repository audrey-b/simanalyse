#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param object An mcmcr or mcmcrs object.
#' @param measures A vector of strings indicating which functions to calculate over the mcmc samples. Strings may include "mean", 
#' "median", "lower.q" (lower bound of the quantile-based credible interval), "upper.q" (upper bound of the quantile-based 
#' credible interval), "var", "sd", "q1" (1st quartile), "q3" (3rd quartile)
#' @param alpha scalar representing the alpha level used to construct credible intervals. Default is 0.05.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the variables in \code{object} and/or \code{code} to monitor. By default all variables are included.
#' @param custom_funs A named list of functions to calculate over the mcmc samples. This optional argument can be used to calculate
#' measures that cannot be specified through \code{measures}. E.g. list(posteriormedian = median).

# @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
# @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
# @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'

# @examples
# set.seed(10L)
# code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
# dat <- sims::sims_simulate(code, parameters = nlist(variance=4), nsims=2)
# res <- sma_analyse_bayesian(dat, code, code.add = "variance ~ dunif(0,10)", 
# n.adapt=100, n.burnin=0, n.iter=3, monitor="variance")
# sma_derive(res, "sd=sqrt(variance)")

sma_summarise <- function(object,
                          measures=c("mean", "lower.q", "upper.q"),
                          monitor=".*",
                          alpha=0.05,
                          custom_funs = list()) {
  
  object=object
  measures=measures
  monitor=monitor
  alpha=alpha
  custom_funs=list(mean=mean, median=median)
  aggregate.FUNS=custom_funs
  
  if(class(object) == "mcmcr") object <- as.nlists(collapse_chains(object))
  if(class(object) == "mcmcrs") object <- lapply(object, function(x) as.nlists(collapse_chains(x)))
  
  lapply(object, summarise_one_result, aggregate.FUNS)
  

}

