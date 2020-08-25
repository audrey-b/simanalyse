#' Calculates posterior predictive checks (Bayesian p-values).
#' 
#' This function is in development. Use at your own risk.
#' 
#' @param object An mcmcr object of results
#' @param expr An expression to calculate the expectations, e.g. "X = mu" if the expectation of X is mu
#' @param data A dataset
#' @param code Code to simulate data
#' @param constants Constants to pass with the code
#' @param nsamples Number of samples to take from the posterior distribution
#' @param statistic A string, one of "FT" (Freeman-Tukey), "LR" (Likelihood Ratio), or "chi2" (Chi-square)
# @param expr.var An expression to calculate the variances, e.g. "X = mu" if the variance of X is mu
#' @return A flag.
#' @export
#'
#' @examples
#' code="for(i in 1:10){
#' a[i] ~ dpois(mu)}"
#' sims <- sims::sims_simulate(code, parameters=nlist(mu=5), nsims=1)
#' res <- sma_analyse(sims, 
#'                            code, 
#'                            "mu ~ dunif(0,10)", 
#'                            monitor = "mu",
#'                            deviance=FALSE,
#'                            mode=sma_set_mode("quick", n.save=300, max.iter=300))
#' sma_assess(res, expr="a = rep(mu, 10)", sims, code, nsamples=10)
#' 
#' 

#parallel option
#omnibus = FALSE TRUE NA (over all parameters)
#A guide to Bayesian model checking for ecologists
#POSTERIOR PREDICTIVE ASSESSMENT OF MODEL FITNESS VIA REALIZED DISCREPANCIES

sma_assess <- function(object, 
                       expr,
                       data,
                       code,
                       constants=nlist(),
                       nsamples=100L,
                       statistic = "FT"){
  
  if(interactive()) yesno::yesno("This function is in development and has not been tested. Are you sure you want to continue?")
  warning("This function is in development and has not been tested. Use at your own risks.")
  
  data <- data[[1]]
  
  nsamples <- as.integer(nsamples)
  
  #collapse chains
  object %<>% 
    mcmcr::as.mcmcr() %>% 
    mcmcr::collapse_chains()
  
  #choose iters to sample
  sampleids <- sample(1:(mcmcr::niters(object)), nsamples, replace=TRUE)
  
  #sample
  sample <- subset(object, iters=sampleids)
  
  #calculate expectations
  expectations <- mcmc_derive(sample, expr=expr, silent=TRUE, values=constants)
  monitor <- names(expectations)
  #names(expectations) <- chk::p0("expectation.", names(expectations))
  
  #discrepancy
  if(statistic == "FT"){fun.stat <- function(data, expectation) (sqrt(data) - sqrt(expectation))^2
  }else if(statistic == "LR"){ fun.stat <- function(data, expectation) 2*data*log(data/expectation)
  }else if(statistic == "chi2"){ fun.stat <- function(data, expectation) (data - expectation)^2/expectation #general case: denom is variance
  }
  
  #calculate Freeman-Tukey statistic with data
  data_mcmcr <- subset(mcmcr::as.mcmcr(data), pars=names(expectations))
  data_mcmcr0 <- data_mcmcr
  
  for(i in 1:(nsamples-1))
    data_mcmcr <- mcmcr::bind_chains(data_mcmcr, data_mcmcr0)
  data_mcmcr <- mcmcr::collapse_chains(data_mcmcr)
  
  D1 <- mcmcr::combine_samples(expectations, data_mcmcr, 
                               fun = function(x) fun.stat(x[2], x[1]))
  
  #calculate Freeman-Tukey statistic with simulated data
  
  simdata <- nlists(nlist())
  for(i in 1L:nsamples){
    simdata[[i]] <- sims_simulate(code, 
                                  constants=constants, 
                                  parameters=as_nlist(subset(sample, iters = i)),
                                  nsims=1,
                                  silent = TRUE)[[1]]
  }
  simdata <- subset(simdata, pars=names(expectations))
  simdata.mcmcr <- mcmcr::as.mcmcr(simdata)
 
  D2 <- mcmcr::combine_samples(expectations, simdata.mcmcr, fun=function(x)fun.stat(x[2],x[1]))
  #names(D2) = names(expectations) #should delete?
  
  #Calculate the bayesian p-value
  
  p <- mcmcr::combine_samples(D1, D2, fun=function(x) as.integer(x[2] > x[1])) #sims>data
  mean.p <- mcmcr::estimates(p, mean)
  
  #Fit (mean D.data)
  
  fit = mcmcr::estimates(D1, mean)
  fit <- lapply(names(fit), function(x) mean(fit[[x]], na.rm = TRUE))
  names(fit) <- names(D1)
  
  return(list(D.data = D1, D.sims = D2, p.value=mean.p, fit=fit))
  
}


