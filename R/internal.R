# make_code <- function(code, append, sprintf_args){}

# analyse_dataset <- function(nlistdata, seed, code, monitor, path, inits, n.adapt, n.burnin, n.iter, thin, quiet) {
#   
#   code <- textConnection(code)
#   
#   inits <- set_seed_inits(seed)
#   
#   model <- rjags::jags.model(code, data = nlistdata, inits = inits, 
#                              
#                              n.adapt = n.adapt, quiet = quiet)
#
#   rjags::update(model, n.iter = n.burnin, progress.bar = "none")
#   
#   sample <- rjags::jags.samples(model, variable.names = monitor, n.iter = n.iter, thin=thin,
#                                 
#                                 progress.bar = "none")
#   
#   nlist <-  set_class(lapply(sample, as_natomic_mcarray), "nlist")
#   
#   if(is.null(path)) return(nlist)
#   
#   saveRDS(nlist, file.path(path, data_file_name(sim)))
#   
#   NULL
#   
# }


# analyse_datasets <- function()