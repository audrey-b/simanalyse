#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. Allows data to be read from files and results to be written to files.
#' 
#' @param sims An nlists or nlist object of the data. If NULL, data files are read from \code{path.read}.
#' @param code A string of code to analyse the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.
#' @param code.values A character vector to pass to sprintf before analysing the data. This is useful for varying choices of distributions, e.g. for assessing sensitivity to the choice of priors.
#' @param package A string with the name of the R package to analyse the data. Currently, only "rjags" is implemented.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param n.chains An integer greater than zero specifying the number of MCMC chains to run
#' @param inits A list or a function. Initial values for the MCMC chains. If specifying a function, it should either have no arguments, or have a single argument named chain. In the latter case, the supplied function is called with the chain number as argument. In this way, initial values may be generated that depend systematically on the chain number.
#' @param n.adapt An integer specifying the number of adaptations for each analysis
#' @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
#' @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
#' @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
#' @param seed An integer. The random seed to use for analysing the data.
#' @param path.read A string. If data is NULL, data is read from that path on disk.
#' @param path.save A string specifying the path to the directory to save the results in. By default path = NULL the results are not saved but are returned as a list of nlists objects.
#' 
#' @return A flag.
#' @export
#'
#' @examples
#'  set.seed(10L)
#'  code <- "a ~ dnorm(mu,1)"
#'  sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#'  prior = "mu ~ dunif(-3,3)"
#'  result <- sma_analyse_bayesian(sims=sims,
#'                                        code = code,
#'                                        code.add = prior,
#'                                        n.adapt = 101,
#'                                        n.burnin = 0,
#'                                        n.iter = 101,
#'                                        monitor = "mu")

sma_analyse_bayesian <- function(sims = NULL,
                                 code,
                                 code.add="",
                                 code.values="",
                                 package="rjags",
                                 monitor, #need to configure so .* by default
                                 inits=list(),
                                 n.adapt,
                                 n.burnin,
                                 n.iter,
                                 thin=1,
                                 n.chains=3,
                                 seed=rinteger(),
                                 path.read = NULL,
                                 path.save = NULL) {
  
  if(!is.null(sims)){
    if(is.nlist(sims)) sims <- nlists(sims)
    chk_is(sims, class="nlists")
    lapply(sims, chk_is, class="nlist")
    n.sims <- length(sims)
  }
  
  
  check_string(code)
  check_string(code.add)
  check_string(code.values)
  check_string(package)
  if(!is.null(path.read)){
    check_string(path.read)
    n.sims <- length(sims_data_files(path.read))
  }
  if(!is.null(path.save)) check_string(path.save)
  
  
  check_character(monitor)
  
  chk_is(inits, class=c("list", "function"))
  #lapply(check_) need to figure out
  
  check_pos_int(n.chains, coerce=TRUE)
  check_pos_int(n.adapt, coerce=TRUE)
  check_noneg_int(n.burnin, coerce=TRUE)
  check_pos_int(n.iter, coerce=TRUE)
  
  check_dbl(thin) #1 to max iter?
  
  check_scalar(seed, c(-.max_integer, .max_integer))
  
  seeds <- rinteger(n.sims)
  
  res.list <- list(nlists(nlist()))
  
  code %<>% prepare_code(code.add, code.values)
  
  #jags
  if(!is.null(sims)){
    for(i in 1:n.sims){
      res.list[[i]] <- analyse_dataset_bayesian(nlistdata=sims[[i]], 
                                                code=code, monitor=monitor,
                                                inits=inits, n.chains=n.chains,
                                                n.adapt=n.adapt, n.burnin=n.burnin, 
                                                n.iter=n.iter, thin=thin,
                                                seed=seeds[i])}
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    return(as.mcmcrs(res.list))
    
  }else{analyse_to_file(code=code, monitor=monitor,
                        inits=inits, n.chains=n.chains,
                        n.adapt=n.adapt, n.burnin=n.burnin, 
                        n.iter=n.iter, thin=thin,
                        seeds=seeds, path.read=path.read, path.save=path.save)  
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
  }
}

