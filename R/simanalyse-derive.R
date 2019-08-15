#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param object A single nlists object or a list of nlists objects.
#' @param code A string of R code to modify the nlists in \code{object}.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the variables in \code{object} and/or \code{code} to monitor. By default all variables are included.
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

sma_derive <- function(object, code, monitor=".*") {

  #need to add checks
  
  derive_one <- function(object.nlists, code, monitor){
  derived_obj <- object.nlists %>% 
    mcmc_derive(expr = code, 
                primary = TRUE, 
                silent = TRUE)
  if(monitor != ".*") return(subset(derived_obj, select=monitor))
  else return(derived_obj)
  }
  
  if(is.nlists(object)) return(derive_one(object, code=code, monitor=monitor))
  else return(lapply(object, derive_one, code=code, monitor=monitor))
  
}

