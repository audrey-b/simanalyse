#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. If path is supplied, saves a hidden file with the information on the analysis.
#' 
#' @param data An nlists or nlist object of the data. If NULL, data files are read from \code{path}.
#' @param code A string of code to analyse the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.
#' @param code.values A character vector to pass to sprintf before analysing the data. This is useful for varying choices of distributions, e.g. for assessing sensitivity to the choice of priors.
#' @param package A string with the name of the R package to analyse the data. Currently, only "rjags" is implemented.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param n.chains An integer greater than zero specifying the number of MCMC chains to run
#' @param inits A list. Initial values for the MCMC chains
#' @param n.adapt An integer specifying the number of adaptations for each analysis
#' @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
#' @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
#' @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
#' @param seed An integer. The random seed to use for analysing the data.
#' @param path A string. If specified, results are saved at that path on disk.
#' 
#' @return A flag.
#' @export
#'
#' @examples
#'  set.seed(10L)
#'  code <- "a ~ dnorm(mu,1)"
#'  dat <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#'  result <- sma_analyse_bayesian(data=dat,
#'                                        code = code,
#'                                        code.add = "mu ~ dunif(-3,3)",
#'                                        n.adapt = 101,
#'                                        n.burnin = 0,
#'                                        n.iter = 101,
#'                                        monitor = "mu")

sma_analyse_bayesian <- function(data = NULL,
                                 code,
                                 code.add="",
                                 code.values="",
                                 package="rjags",
                                 monitor,
                                 inits=list(),
                                 n.adapt,
                                 n.burnin,
                                 n.iter,
                                 thin=1,
                                 n.chains=3,
                                 seed=rinteger(),
                                 path = NULL) {
  
  if(!is.null(data)){
  if(is.nlist(data)) data <- nlists(data)
  check_nlists(data)
  lapply(data, check_nlist)
  }
  
  check_string(code)
  check_string(code.add)
  check_string(code.values)
  check_string(package)
  if(!is.null(path)) check_string(path)
  
  check_character(monitor)
  
  check_list(inits)
  #lapply(check_) need to figure out
  
  check_pos_int(n.chains, coerce=TRUE)
  check_pos_int(n.adapt, coerce=TRUE)
  check_noneg_int(n.burnin, coerce=TRUE)
  check_pos_int(n.iter, coerce=TRUE)
  
  check_dbl(thin) #1 to max iter?
  
  check_scalar(seed, c(-.max_integer, .max_integer))
  
  n.data <- length(data)
  
  seeds <- rinteger(n.data)
  
  res.list <- list(nlists(nlist()))
  
  code %<>% prepare_code(code.add, code.values)
  
  #jags
  if(!is.null(data)){
  for(i in 1:n.data){
    res.list[[i]] <- analyse_dataset_bayesian(nlistdata=data[[i]], 
                                              code=code, monitor=monitor,
                                              inits=inits, n.chains=n.chains,
                                              n.adapt=n.adapt, n.burnin=n.burnin, 
                                              n.iter=n.iter, thin=thin,
                                              seed=seeds[i])}
  return(res.list)
  }else analyse_to_file(code=code, monitor=monitor,
                  inits=inits, n.chains=n.chains,
                  n.adapt=n.adapt, n.burnin=n.burnin, 
                  n.iter=n.iter, thin=thin,
                  seed=seed, path=path) #will this use the same seeds as above??? I think this will need to be fixed
  
}
