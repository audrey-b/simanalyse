#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. If path is supplied, saves a hidden file with the information on the analysis.
#' 
#' @param datalist A list of nlist objects containing the data. Alternatively, path can be used to specify to read the data from files.
#' @param package A string with the name of the R package to analyse the data, among the following ones: "rjags"
#' @param code A string of code to analyse the data. JAGS code must not be in a data or model block.
#' @param code_add A string of code to add at the end of \code{code} before analysing the data. This is useful for specifying priors seperately from the likelihood in JAGS.
#' @param code_values A character vector to pass to sprintf when applied to the appended code from \code{code} and \code{code_add}. This is useful for varying choices of distributions or conducting sensitivity to prior analyses.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param n.chains An integer greater than zero specifying the number of mcmc chains to run
#' @param inits Initial values for the mcmc chains
#' @param n.adapt An integer specifying the number of adaptations for each analysis
#' @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
#' @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
#' @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
#' @param seed An integer specifying the random seed to use for analysing the data.
#' 
#' @return A flag.
#' @export
#'
#' @examples
#'  set.seed(10L)
#'  code <- "a ~ dnorm(mu,1)"
#'  dat <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#'  result <- simanalyse_analyse_bayesian(datalist=dat,
#'                                        code = code,
#'                                        code_add = "mu ~ dunif(-3,3)",
#'                                        n.adapt = 101,
#'                                        n.burnin = 0,
#'                                        n.iter = 101,
#'                                        monitor = "mu")

simanalyse_analyse_bayesian <- function(datalist,
                                        code,
                                        code_add="",
                                        code_values="",
                                        package="rjags",
                                        monitor,
                                        inits=list(),
                                        n.adapt,
                                        n.burnin,
                                        n.iter,
                                        thin=1,
                                        n.chains=3,
                                        seed=rinteger()) {
  
  
  check_nlists(datalist)
  lapply(datalist, check_nlist)
  
  check_string(code)
  check_string(code_add)
  check_string(code_values)
  check_string(package)
  
  check_character(monitor)
  
  check_list(inits)
  #lapply(check_) need to figure out
  
  check_pos_int(n.chains, coerce=TRUE)
  check_pos_int(n.adapt, coerce=TRUE)
  check_noneg_int(n.burnin, coerce=TRUE)
  check_pos_int(n.iter, coerce=TRUE)
  
  check_dbl(thin) #1 to max iter?
  
  check_scalar(seed, c(-.max_integer, .max_integer))
  
  n.data <- length(datalist)
  
  seeds <- rinteger(n.data)
  
  res.list <- list(nlists(nlist()))
  
  code %<>% prepare_code(code_add, code_values)
  
  #jags
  for(i in 1:n.data){
    res.list[[i]] <- analyse_dataset_bayesian(nlistdata=datalist[[i]], 
                                              code=code, monitor=monitor,
                                              inits=inits, n.chains=n.chains,
                                              n.adapt=n.adapt, n.burnin=n.burnin, 
                                              n.iter=n.iter, thin=thin,
                                              seed=seeds[i])}
  return(res.list)
  
}
