#' Apply R code to derive new variables.
#' 
#' Apply R code to derive new variables.
#' 
#' @param object One of either nlists, mcmc or mcmc.list or a list of those. If \code{parameters} is specified, this code must only transform variables present in \code{parameters}.
#' @param code A string of R code to derive posterior samples for new parameters. E.g. "var = sigma^2".
#' @param monitor A character vector (or regular expression if a string) specifying the names of the variables in \code{object} and/or \code{code} to monitor. By default all variables are included.
#' @param progress A flag specifying whether to print a progress bar.
#' @param options The future specific options to use with the workers.

# @param append A flag indicating whether to return the derived parameters along with the original ones.
# @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
# @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
# @param silent A flag specifying whether to suppress warnings.

#' @return An object of the same class as \code{object}
#' @export
#'
#' @examples
#' set.seed(10L)
#' code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
#' parameters = nlist(variance=4)
#' dat <- sims::sims_simulate(code, parameters = parameters, nsims=2)
#' res <- sma_analyse_bayesian(dat, code, code.add = "variance ~ dunif(0,10)", 
#' n.adapt=100, n.burnin=0, n.iter=3, monitor="variance")
#' sma_derive(res, "sd=sqrt(variance)")
#' sma_derive(parameters, "sd=sqrt(variance)")

sma_derive <- function(object, code, monitor=".*", progress = FALSE,
                       options = furrr::future_options()) {
  
  #do not monitor non-primary variables that are not in monitor
  monitor.non.primary <- ".*" 
  if(!(".*" %in% monitor)){
  if(class(object) %in% c("nlist", "mcmcr")){
    primary.params <- names(object)
    }else{
      primary.params <- names(object[[1]])
    }
    
  monitor.non.primary <- monitor[!(monitor %in% primary.params)]
  }
  
  #if(length(monitor.non.primary) > 1) monitor <- paste(monitor.non.primary, collapse=" | ") #make regular expression

  if(class(object)=="mcmcrs"){
    new_obj <- future_map(object, mcmc_derive, expr=code, monitor=monitor.non.primary, primary=TRUE, .progress=progress, .options=options)
    if(monitor!=".*") new_obj <- future_map(new_obj, subset, pars=monitor, .progress=progress, .options=options) #remove primary that are not in monitor
  }else if(class(object)=="mcmcr" | class(object)=="nlist"){
    new_obj <- mcmc_derive(object, code, monitor=monitor.non.primary, primary=TRUE)    
    if(monitor!=".*") new_obj <- subset(new_obj, pars=monitor) #remove primary that are not in monitor
  }
  
  return(new_obj)
         
}

