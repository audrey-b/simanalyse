#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param object A list of nlists object of results or a single nlists of results. If \code{parameters} is specified, this code must only transform variables present in \code{parameters}.
#' @param code A string of R code to derive posterior samples for new parameters. E.g. "var = sigma^2".
#' @param measures A vector of strings indicating which functions to calculate over the mcmc samples. Strings may include "mean", 
#' "median", "lower.q" (lower bound of the quantile-based credible interval), "upper.q" (upper bound of the quantile-based 
#' credible interval), "var", "sd", "q1" (1st quartile), "q3" (3rd quartile)
#' @param parameters An nlist of the true values of the parameters in \code{object}. Optional, but must be passed if \code{sma_performance} is to be used.
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
#' @examples
#' set.seed(10L)
#' code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
#' dat <- sims::sims_simulate(code, parameters = nlist(variance=4), nsims=2)
#' res <- sma_analyse_bayesian(dat, code, code.add = "variance ~ dunif(0,10)", 
#' n.adapt=100, n.burnin=0, n.iter=3, monitor="variance")
#' sma_derive(res, "sd=sqrt(variance)")

sma_derive <- function(object, code, 
                          measures=c("mean", "lower.q", "upper.q"), 
                          parameters=nlist::nlist(), monitor=".*", alpha=0.05,
                          custom_funs = list()) {
  
  #need to add checks
  parameters=parameters #tmp
  measures=measures #tmp
  custom_funs=custom_funs #tmp
  
  #apply code to parameters to derive new variables
  
  if(length(parameters)!=0) derived.parameters <- derive_one(parameters, code=code)
  
  #apply code to object to derive new variables, keep only monitor
  
  if(is.nlists(object)){
    derived.object <- derive_one(object, code=code) 
    if(length(parameters)!=0) derived.object %<>% c(derived.parameters)
    if(monitor != ".*"){derived.object %<>% subset(select=monitor)}
  } else{
    derived.object <- lapply(object, derive_one, code=code)
    if(length(parameters)!=0) derived.object %<>% lapply(obj=derived.parameters)
    if(monitor != ".*"){derived.object %<>% lapply(subset, select=monitor)}
  }
  return(derived.object)
  #apply measures and custom_funs to calculate summaries over the derived object
  
  
  
  
  #combine the two nlist with c() through lapply on index i when parameters are used
  
  
  
}

