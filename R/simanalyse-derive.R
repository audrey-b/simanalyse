#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param object A nlists object
#' @param code A string of R code to modify the results.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the data. By default all stochastic nodes are included.
# @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
# @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
# @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'
#' @examples
#' set.seed(10L)
#' code <- "for(i in 1:10){x[i] ~ dnorm(mu,1)}"
#' dat <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#' sma_derive(dat, "y=x+1")

sma_derive <- function(object, code, monitor=".*") {

  #need to add checks
  #need to allow object to be a list and apply to each nlists element
  
  derived_obj <- object %>% 
    mcmc_derive(expr = code, 
                primary = TRUE, 
                silent = TRUE)
  if(monitor != ".*") return(subset(derived_obj, select=monitor))
  else return(derived_obj)
}

