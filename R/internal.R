# prepare_code <- function(code, append=NULL, ...){
#   
#   if(str_detect(code, "^\\s*(data)|(model)\\s*[{]"))
#     err("jags code must not be in a data or model block")
#   
#   code %>% 
#     paste(append, sep = " \n ") %>% 
#     sprintf(...) %>%
#     add_model_block()
#   
# }

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
  
  code %<>% add_model_block() %>% textConnection
  
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

make_expr_and_FUNS <- function(measures, parameters, estimator, alpha, custom_expr="", custom_FUNS=NULL){
  expr <- NULL
  aggregate.FUNS <- NULL
  if(("bias" %in% measures) | ("mse" %in% measures)) {
    aggregate.FUNS %<>% append(list(estimator = estimator))
  }
  if("bias" %in% measures){
    expr=paste(c(expr, "bias = estimator - parameters"), collapse=" \n ", sep="")
  }
  if("mse" %in% measures){
    expr=paste(c(expr, "mse = (estimator - parameters)^2"), collapse=" \n ", sep="")
  }
  if("cp.quantile" %in% measures){
    expr=paste(c(expr, "cp.quantile = ifelse((parameters >= cp.low) & (parameters <= cp.high), 1, 0)"), collapse=" \n ", sep="")
    cp.low.with.alpha = function(x) do.call("cp.low",list(x,"alpha"=alpha))
    cp.high.with.alpha = function(x) do.call("cp.high",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list(cp.low = cp.low.with.alpha, cp.high = cp.high.with.alpha))
  }
  if(custom_expr!="") expr=paste(c(expr, custom_expr), collapse=" \n ", sep="")
  if(!is.null(custom_FUNS)) aggregate.FUNS %<>% append(custom_FUNS)

  return(list(expr=expr, aggregate.FUNS=aggregate.FUNS))
}

summarise_all_measures <- function(listnlists, 
                                   expr_FUNS, 
                                   parameters){
  
  listnlists %>% 
    lapply(summarise_within, expr_FUNS[["expr"]], expr_FUNS[["aggregate.FUNS"]], parameters) %>%
    as.nlists() %>%
    summarise_across(mean) %>%
    return
}

summarise_across <- function(summary.nlist, FUN){
  summary.nlist %>% 
    aggregate(FUN = mean) %>%
    return
}



summarise_within <- function(nlists, expr, aggregate.FUNS, parameters){
  monitor = nlists[[1]] %>% names()
  #calculate aggregates
  aggregate.list <- aggregate.FUNS %>% 
    lapply(function(FUN, nlists) aggregate(nlists, FUN), nlists) %>%
    unlist(recursive=FALSE) %>% #now names are of the form estimator.mu
    as.nlist()
  #apply expr
  names(parameters) <- paste0("parameters.",names(parameters))
  measures <- measure_names(expr)
  keywords <- c(measures, names(aggregate.FUNS), "parameters")
  expr.all.params <- names(nlists[[1]]) %>% 
    lapply(make_one_expr, expr, keywords) %>%
    unlist %>%
    paste(collapse=" \n ")
  mcmc_derive(aggregate.list, expr.all.params, parameters) %>%
    return
}

make_one_expr <- function(param, expr, keywords){
  patterns <- str_c("\\b", keywords, "\\b", collapse="|")
  str_replace_all(expr, 
                  patterns, 
                  function(x) if(x %in% keywords) return(p0(x,".",param)))
}

strip_comments <- function(x) {
  str_replace_all(x, pattern = "\\s*#[^\\\n]*", replacement = "")
}

measure_names <- function (x) {
  x <- strip_comments(x)
  
  pattern <- "(?=\\s*([=]|([<][-])))"
  
  index <- "\\[[^\\]]*\\]"
  
  pattern <- p0("\\w+(", index, "){0,1}\\s*[)]{0,1}", pattern, collapse = "")
  nodes <- str_extract_all(x, pattern)
  nodes <- unlist(nodes)
  nodes <- str_replace(nodes, pattern = "[)]$", "")
  nodes <- str_replace(nodes, pattern = "\\s*$", "")
  nodes <- str_replace(nodes, pattern = index, "")
  nodes <- unique(nodes)
  sort(nodes)
}

prepare_code <- function(code, code_add, code_values){
  code <- paste(c(code, code_add), 
                collapse=" \n", 
                sep="")
  if(str_detect(code, "^\\s*(data)|(model)\\s*[{]"))
    err("jags code must not be in a data or model block")
  
  do.call(sprintf, args = as.list(c(code, code_values))) %>% 
    return
}

