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
#'                            mode=sma_set_mode("quick", n.save=300, max.iter=300))
#' sma_assess(res, "a = mu", sims, code)

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
  
  warning("This function is in development. Use at your own risk.")
  
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
  expectations <- mcmc_derive(sample, expr=expr)
  monitor <- names(expectations)
  names(expectations) <- chk::p0("expectation.", names(expectations))
  
  #statistic
  if(statistic == "FT"){expr.stat <- "(sqrt(data) - sqrt(expectation))^2"
  }else if(statistic == "LR"){ expr.stat <- "2*data*log(data/expectation)"
  }else if(statistic == "chi2"){ expr.stat <- "(data - expectation)^2/expectation" #general case: denom is variance
  }
  
  #calculate Freeman-Tukey statistic with data
  expr.FT.1 <- chk::p0("D1 =", expr.stat)
  all.expr.FT.1 <- expand_expr(expr.FT.1, c("data", "expectation"), monitor, monitor)
  names(data) <- paste0("data.",names(data))
  D1 <- mcmc_derive(expectations, all.expr.FT.1, values = data)
  
  #calculate Freeman-Tukey statistic with simulated data
  
  simdata <- nlists(nlist())
  for(i in 1L:nsamples){
    simdata[[i]] <- sims_simulate(code, 
                                  constants=constants, 
                                  parameters=as.nlist(subset(sample, iters = i)),
                                  nsims=1)[[1]]
  }
  expr.FT.2 <- "D2 = (sqrt(data) - sqrt(expectation))^2"
  all.expr.FT.2 <- expand_expr(expr.FT.2, c("data", "expectation"), monitor, monitor)
  simdata.mcmcr <- mcmcr::as.mcmcr(simdata)
  names(simdata.mcmcr) <- paste0("data.", names(simdata[[1]]))
  obj <- mcmcr::bind_parameters(expectations, simdata.mcmcr)
  D2 <- mcmc_derive(obj, all.expr.FT.2)
  
  #Calculate the bayesian p-value
  
  all.expr.p <- expand_expr("p = as.integer(D2 > D1)", c("D1", "D2"), monitor, monitor)
  zeroones <- mcmcr::bind_parameters(D1, D2) %>% 
    mcmc_derive(all.expr.p) %>%
    as.nlists
  p <- aggregate(zeroones, fun=mean)
  
  return(p)
  
}


