#' Apply R code to derive new variables.
#' 
#' Apply R code to derive new variables.
#' 
#' @param object An mcmcrs object or an object that can be coerced to mcmcrs such as list, nlist, nlists, mcmc or mcmc.list. If set to NULL, the object is read from \code{path} instead.
#' @param code A string of R code to derive posterior samples for new parameters. E.g. "var = sigma^2".
#' @param monitor A character vector (or regular expression if a string) specifying the names of the variables in \code{object} and/or \code{code} to monitor. By default all variables are included.
#' @param values A named list of additional R objects to evaluate in the R expression.
#' @param path A string. If object is NULL, analyses results are read from that path on disk.
#' @param analysis If \code{path} is used, a string for the name of the folder that contains the analysis.
#' @param progress A flag specifying whether to print a progress bar.
#' @param options The future specific options to use with the workers.

# @param append A flag indicating whether to return the derived parameters along with the original ones.
# @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
# @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
# @param silent A flag specifying whether to suppress warnings.
# @param path.save A string specifying the path to the directory to save the derived results. By default path = NULL and the results are not saved but returned as a list of nlists objects.

#' @return An object of the same class as \code{object}
#' @export
#'
#' @examples
#' set.seed(10L)
#' code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
#' parameters = nlist(variance=4)
#' dat <- sims::sims_simulate(code, parameters = parameters, nsims=2)
#' res <- sma_analyse_bayesian(dat, code, code.add = "variance ~ dunif(0,10)", 
#' mode=sma_set_mode("quick"), monitor="variance")
#' sma_derive(res, "sd=sqrt(variance)")
#' sma_derive(parameters, "sd=sqrt(variance)")


sma_derive <- function(object=NULL, code, monitor=".*", 
                       values=list(),
                       path = ".",
                       analysis = "analysis0000001",
                       progress = FALSE,
                       options = furrr::future_options()) {
  
  
  if(!mcmcr::is.mcmcr(object) && !mcmcr::is.mcmcr(object) && length(lengths(object))==1){
    object <- mcmcr::as.mcmcr(object)
    mcmcr::chk_mcmcr(object)}
  if(!is_nlist(object) && !is_nlists(object) && length(lengths(object))>1){
    object <- mcmcr::as.mcmcrs(object)
    mcmcr::chk_mcmcrs(object)
  }
  # chk_nlists(object)
  # 
  # if(!is_nlist(object) && !is_nlists(object) && length(lengths(object))==1){
  #   class(object) <- "nlist"
  #   chk_nlist(object)
  #   object <- nlists(object)}
  # if(!is_nlist(object) && !is_nlists(object) && length(lengths(object))>1){
  #   class(object) <- "nlists"
  #   for(i in 1:length(object)) class(object[[i]]) <- "nlist"
  # }
  # chk_nlists(object)
  # 
  # 
  #do not monitor non-primary variables that are not in monitor
  monitor.non.primary <- ".*" 
  if(!(".*" %in% monitor)){
    if(class(object) %in% c("nlist")){
      primary.params <- names(object)
    }else{
      primary.params <- names(object[[1]])
    }
    
    monitor.non.primary <- monitor[!(monitor %in% primary.params)]
  }
  
  
  
  #if(length(monitor.non.primary) > 1) monitor <- paste(monitor.non.primary, collapse=" | ") #make regular expression
  
  if(!is.null(object)){
    #files <- list.files(path, pattern = "^results\\d{7,7}[.]rds$")
    #object <- lapply(file.path(path, files), readRDS)
    sma_derive_internal(object, code, monitor, values, monitor.non.primary, progress, options)
    
  }else{
    chk_dir(path)
    
    sma_batchr(sma.fun=sma_derive_internal, 
               analysis=analysis,
                   path.read = file.path(path, analysis, "results"),
                   path.save = file.path(path, analysis, "derived"),
                   prefix="results", suffix="deriv",
                   code=code, monitor=monitor, values=values,
                   monitor.non.primary=monitor.non.primary,
                   progress=progress, options=options) #need to change
    
    parameters <- sims_info(path)$parameters
    derived.params <- sma_derive_internal(parameters, code, monitor, values, monitor.non.primary, progress, options)
    saveRDS(derived.params, file.path(path, analysis, "derived", ".parameters.rds"))
  }
}
