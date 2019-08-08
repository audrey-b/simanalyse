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
  
  inits$.RNG.seed <- as.integer(runif(1, 0, .max_integer))
  
  inits
  
}

analyse_dataset_bayesian <- function(nlistdata, code, monitor, 
                                     path=NULL, inits=list(), n.adapt, n.burnin, 
                                     n.iter, thin=1, seed = sims::rcount(), quiet = FALSE) {

  code <- code %>% prepare_code() %>% textConnection

  inits <- set_seed_inits(seed, inits)

  model <- rjags::jags.model(code, data = nlistdata, inits = inits,

                             n.adapt = n.adapt, quiet = quiet)

  if(n.burnin >= 1) update(model, n.iter = n.burnin, progress.bar = "none")

  sample <- rjags::jags.samples(model, variable.names = monitor, n.iter = n.iter, thin=thin,

                                progress.bar = "none")

  nlist <-  set_class(lapply(sample, as_natomic_mcarray), "nlist")

  if(is.null(path)) return(nlist)

  #saveRDS(nlist, file.path(path, data_file_name(sim)))
  #data_file_name <- function(sim) p0("data", sprintf("%07d", sim), ".rds")
  NULL

}

as_natomic_mcarray <- function(x) {
  
  dim <- dim(x)
  
  ndim <- length(dim)
  
  x <- as.vector(x)
  
  dim(x) <- dim(x)[-c(ndim-1L,ndim)]
  
  x
  
}

# analyse_datasets_bayesian <- function(nlistsdata)