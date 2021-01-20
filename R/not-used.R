# #playing around
# 
# sma_assess2 <- function(object, 
#                        data,
#                        code,
#                        constants=nlist(),
#                        nsamples=NA,
#                        statistic = "FT"){
#   
#   if(interactive()) yesno::yesno("This function is in development and has not been tested. Are you sure you want to continue?")
#   warning("This function is in development and has not been tested. Use at your own risks.")
#   
#   data <- data[[1]]
#   
#   if(is.na(nsamples)){nsamples=niters(object)
#   replace=FALSE
#   }else if(nsamples <= niters(object)){
#     replace=FALSE}else{replace=TRUE}
#   nsamples <- as.integer(nsamples)
#   
#   #collapse chains
#   object %<>% 
#     mcmcr::as.mcmcr() %>% 
#     mcmcr::collapse_chains()
#   
#   #choose iters to sample
#   sampleids <- sample(1:(mcmcr::niters(object)), nsamples, replace=replace)
#   
#   #sample
#   sample <- subset(object, iters=sampleids)
#   
#   #simulate data (draw from posterior and simulate a dataset for each, Cohn et al.)
#   simdata <- nlists(nlist())
#   for(i in 1L:nsamples){
#     simdata[[i]] <- sims_simulate(code, 
#                                   constants=constants, 
#                                   parameters=as_nlist(subset(sample, iters = i)),
#                                   nsims=1,
#                                   latent=FALSE,
#                                   silent = TRUE)[[1]]
#   }
#   #simdata <- subset(simdata, pars=names(expectations))
#   simdata.mcmcr <- mcmcr::as.mcmcr(simdata)
#   
#   #calculate expectations
#   expectations <- estimates(simdata.mcmcr, fun=mean)
#   expectations <- as.mcmcr(as_nlists(rep(list(expectations), niters(simdata.mcmcr))))
#   
#   #expectations <- mcmc_derive(sample, expr=expr, silent=TRUE, values=constants)
#   #monitor <- names(expectations)
#   #names(expectations) <- chk::p0("expectation.", names(expectations))
#   
#   #discrepancy
#   if(statistic == "FT"){fun.stat <- function(data, expectation) (sqrt(data) - sqrt(expectation))^2
#   }else if(statistic == "LR"){ fun.stat <- function(data, expectation) 2*data*log(data/expectation)
#   }else if(statistic == "chi2"){ fun.stat <- function(data, expectation) (data - expectation)^2/expectation #general case: denom is variance
#   }
#   
#   #calculate Freeman-Tukey statistic with data
#   data_mcmcr <- subset(mcmcr::as.mcmcr(data), pars=names(expectations))
#   data_mcmcr <- as.mcmcr(as_nlists(rep(list(data_mcmcr), niters(simdata.mcmcr))))
#   
#   D1 <- mcmcr::combine_samples(expectations, data_mcmcr, 
#                                fun = function(x) fun.stat(x[2], x[1]))
#   
#   #calculate Freeman-Tukey statistic with simulated data
#   D2 <- mcmcr::combine_samples(expectations, simdata.mcmcr, fun=function(x)fun.stat(x[2],x[1]))
#   #names(D2) = names(expectations) #should delete?
#   
#   #Calculate the bayesian p-value
#   
#   #niters(D1)
#   #pdims(D1)
#   #names(pdims(D1))
#   D2.unlist <- unlist(D2)
#   D2.pool <- sapply(1:niters(D2), function(i) sum(D2.unlist[seq(i,length(D2.unlist),by=niters(D2))], na.rm=TRUE))
#   
#   D1.unlist <- unlist(D1)
#   D1.pool <- sapply(1:niters(D1), function(i) sum(D1.unlist[seq(i,length(D1.unlist),by=niters(D1))], na.rm=TRUE))
#   
#   mean(D2.pool>D1.pool)
#   
#   p <- mcmcr::combine_samples(D1, D2, fun=function(x) as.integer(x[2] > x[1])+0.5*as.integer(x[2] == x[1])) #sims>data
#   mean.p <- mcmcr::estimates(p, mean)
#   
#   #Fit (mean D.data)
#   
#   fit = mcmcr::estimates(D1, mean)
#   fit <- lapply(names(fit), function(x) mean(fit[[x]], na.rm = TRUE))
#   names(fit) <- names(D1)
#   
#   return(list(D.data = D1, D.sims = D2, p.value=mean.p, fit=fit))
#   
# }
# 
# 
