prepare_code <- function(code, append=NULL, ...){
  
  if(str_detect(code, "^\\s*(data)|(model)\\s*[{]"))
    err("jags code must not be in a data or model block")
  
  code %>% 
    paste(append, sep = " \n ") %>% 
    sprintf(...) %>%
    add_model_block()
  
}

add_model_block <- function(code){
  
  sprintf("model{\n\n%s\n\n}", code)
  
}


set_seed_inits <- function(seed, inits) {
  
  set.seed(seed)
  
  inits$.RNG.name <- "base::Wichmann-Hill"
  
  inits$.RNG.seed <- abs(as.integer(rinteger()))
  
  inits
  
}

analyse_dataset_bayesian <- function(nlistdata, code, monitor, 
                                     n.chains=3, inits=list(), n.adapt, n.burnin, 
                                     n.iter, thin=1, seed = rinteger(), 
                                     quiet = FALSE) {
  
  code <- code %>% prepare_code() %>% textConnection
  
  inits <- set_seed_inits(seed, inits)
  
  model <- rjags::jags.model(code, data = nlistdata, inits = inits,
                             
                             n.adapt = n.adapt, n.chains = n.chains, quiet = quiet)
  
  if(n.burnin >= 1) update(model, n.iter = n.burnin)
  
  sample <- rjags::jags.samples(model, variable.names = monitor, n.iter = n.iter, thin=thin)
  
  nlists <-  sample %>% as.mcmcr() %>% collapse_chains() %>% as.nlists
  
  return(nlists)
  #saveRDS(nlist, file.path(path, data_file_name(sim)))
  #data_file_name <- function(sim) p0("data", sprintf("%07d", sim), ".rds")
  
  
}

as_natomic_mcarray <- function(x) {
  
  dim <- dim(x)
  
  ndim <- length(dim)
  
  x <- as.vector(x)
  
  dim(x) <- dim(x)[-c(ndim-1L,ndim)]
  
  x
  
}

# analyse_datasets_bayesian <- function(nlistsdata)

summarise_one_measure <- function(results.nlists, 
                                  measure, 
                                  estimator, 
                                  parameters){
  if(measure == "Epvar"){ 
    aggregate_FUN = var
    measure_FUN = NULL
  }else if(measure == "Epsd"){ 
    aggregate_FUN = sd
    measure_FUN = NULL
  }else aggregate_FUN = mean
  
  if(measure=="bias") measure_FUN = function(x, param.value){
    estimator(x) - param.value}
  #if(measure=="rb") measure_FUN = function(x, param.value){
  #  (estimator(x) - param.value)/param.value}
  
  results.nlists %>% 
    summarise_within(measure_FUN, aggregate_FUN, parameters) %>%
    summarise_across(mean) %>%
    return
}

summarise_within <- function(results.nlists, 
                             measure_FUN=NULL, 
                             aggregate_FUN,
                             parameters=NULL){
  if(!is.null(measure_FUN)){
    monitor <- results.nlists[[1]][[1]] %>% names()
    pos <- 1
    envir = as.environment(pos)
    assign("measure_FUN", measure_FUN, envir = envir) #not ideal, quick fix
    expr <- paste("summary_", monitor, " <- measure_FUN(",monitor,", ",monitor,".value",")", collapse=" \n ", sep="")
    names(parameters) <- paste0(names(parameters), ".value")
    results.nlists[[1]] %>% mcmc_derive(expr=expr, values=parameters)
  }
  
  summary.nlist <- lapply(results.nlists, aggregate, aggregate_FUN) %>% #, mcmc_derive, expr=expr)
    as.nlists()
  return(summary.nlist)
}

summarise_across <- function(summary.nlist, FUN){
  summary.nlist %>% 
    aggregate(FUN = mean) %>%
    return
}
