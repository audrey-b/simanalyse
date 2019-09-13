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
  
  chk_is(object, class=c("mcmcr", "mcmcrs"))
  chk_vector(measures); chk_all(measures, chk_string)
  chk_string(monitor)
  chk_number(alpha); chk_range(alpha)
  chk_list(custom_funs); chk_all(custom_funs, chk_function)

  aggregate.FUNS = list()
    
  if("mean" %in% measures){
    aggregate.FUNS %<>% append(list("mean"=mean))
  }
  if("median" %in% measures){
    aggregate.FUNS %<>% append(list("median"=median))
  }
  if("var" %in% measures){
    aggregate.FUNS %<>% append(list("var"=var))
  }
  if("sd" %in% measures){
    aggregate.FUNS %<>% append(list("sd"=sd))
  }
  if("lower.q" %in% measures){
    lower.q = function(x) do.call("cp.lower",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list("lower.q"=lower.q))
  }
  if("upper.q" %in% measures){
    upper.q = function(x) do.call("cp.upper",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list("upper.q"=lower.q))
  }
  if("q1" %in% measures){
    aggregate.FUNS %<>% append(list("q1"= function(x) quantile(x, 0.25)))
  }
  if("q3" %in% measures){
    aggregate.FUNS %<>% append(list("q3"= function(x) quantile(x, 0.75)))
  }
  
  if(length(custom_funs)>0) aggregate.FUNS %<>% append(custom_funs)
  
  
  if(monitor != ".*") object <- subset(object, pars=monitor)
  if(class(object) == "mcmcr") object <- as.nlists(mcmcr::collapse_chains(object))
  if(class(object) == "mcmcrs") object <- lapply(object, function(x) as.nlists(mcmcr::collapse_chains(x)))
  
  lapply(object, summarise_one_result, aggregate.FUNS)
  

}

