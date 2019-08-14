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

make_expr_and_FUNS <- function(measures, 
                               parameters, 
                               estimator, 
                               alpha, 
                               custom_FUNS=NULL,
                               custom_expr_before="", 
                               custom_expr_after=""){
  expr <- NULL
  aggregate.FUNS <- NULL
  derive_expr <- NULL
  
  #aggregate_FUNS
  if(sum(c("bias", "mse", "rb", "br", "var", "se", "rmse", "rrmse", "all") %in% measures) > 0) {
    aggregate.FUNS %<>% append(list(estimator = estimator))
  }
  if(sum(c("Epvar", "all") %in% measures) > 0){
    aggregate.FUNS %<>% append(list(Epvar = var))
  }
  if(sum(c("Epse", "all") %in% measures) > 0){
    aggregate.FUNS %<>% append(list(Epse = sd))
  }
  if(sum(c("cp.quantile", "cp.length", "all") %in% measures) > 0){
    cp.low.with.alpha = function(x) do.call("cp.low",list(x,"alpha"=alpha))
    cp.high.with.alpha = function(x) do.call("cp.high",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list(cp.low = cp.low.with.alpha, cp.high = cp.high.with.alpha))
  }
  
  #expr
  if(sum(c("bias", "rb", "br", "var", "se", "all") %in% measures) > 0){
    expr=paste(c(expr, "bias = estimator - parameters"), collapse=" \n ", sep="")
  }
  if(sum(c("mse", "br", "var", "se", "rmse", "rrmse", "all") %in% measures) > 0){
    expr=paste(c(expr, "mse = (estimator - parameters)^2"), collapse=" \n ", sep="")
  }
  if(sum(c("cp.quantile", "all") %in% measures) > 0){
    expr=paste(c(expr, "cp.quantile = ifelse((parameters >= cp.low) & (parameters <= cp.high), 1, 0)"), collapse=" \n ", sep="")
  }
  if(sum(c("E", "cv", "all") %in% measures) > 0){
    expr=paste(c(expr, "E = estimator"), collapse=" \n ", sep="")
  }
  if(sum(c("cp.length", "all") %in% measures) > 0){
    expr=paste(c(expr, "cp.length = cp.high - cp.low"), collapse=" \n ", sep="")
  }
  
  #derive
  if(sum(c("rb", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "rb = bias/parameters"), collapse=" \n ", sep="")
  }
  if(sum(c("var", "br", "se", "cv", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "var = mse - bias^2"), collapse=" \n ", sep="")
  }
  if(sum(c("br", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "br = bias/sqrt(var)"), collapse=" \n ", sep="")
  }
  if(sum(c("se", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "se = sqrt(var)"), collapse=" \n ", sep="")
  }
  if(sum(c("cv", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "cv = sqrt(var)/E"), collapse=" \n ", sep="")
  }
  if(sum(c("rmse", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "rmse = sqrt(mse)"), collapse=" \n ", sep="")
  }
  if(sum(c("rrmse", "all") %in% measures) > 0){
    derive_expr = paste(c(derive_expr, "rrmse = sqrt(mse)/parameters"), collapse=" \n ", sep="")
  }
  
  if(custom_expr_before!="") expr=paste(c(expr, custom_expr_before), collapse=" \n ", sep="")
  if(custom_expr_after!="") derive_expr=paste(c(derive_expr, custom_expr_after), collapse=" \n ", sep="")
  if(!is.null(custom_FUNS)) aggregate.FUNS %<>% append(custom_FUNS)
  
  return(list(expr=expr, aggregate.FUNS=aggregate.FUNS, derive_expr=derive_expr))
}

summarise_all_measures <- function(listnlists, 
                                   expr_FUNS, 
                                   parameters){
  
  listnlists %>% 
    lapply(summarise_within, expr_FUNS[["expr"]], 
           expr_FUNS[["aggregate.FUNS"]], 
           parameters) %>%
    as.nlists() %>%
    summarise_across(mean) %>%
    derive_measures(expr_FUNS[["derive_expr"]], 
                    measure_names(expr_FUNS[["expr"]]), 
                    parameters) %>%
    #summary_reformat(measures=measures,
    #                 monitor=names(listnlists)[[1]]) %>%
    return
}

derive_measures <- function(nlist, derive_expr, keywords, parameters){
  if(!is.null(derive_expr)){
    monitor = names(parameters)
    expr.all.params <- expand_expr(derive_expr, keywords, monitor, parameters)
    names(parameters) <- paste0("parameters.",names(parameters))
    mcmc_derive(nlist, expr.all.params, parameters, silent = TRUE) %>%
      append(nlist) %>%
      return
  }else{return(nlist)}
}

summarise_across <- function(summary.nlist, FUN){
  summary.nlist %>% 
    aggregate(FUN = mean) %>%
    return
}

expand_expr <- function(expr, keywords, monitor, parameters){
  measures <- measure_names(expr)
  keywords %<>% c(measures, "parameters")
  expr.all.params <- monitor %>% 
    lapply(make_one_expr, expr, keywords) %>%
    unlist %>%
    paste(collapse=" \n ") %>%
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
  if(!is.null(expr)){
    expr.all.params <- expand_expr(expr, names(aggregate.FUNS), monitor, parameters)
    names(parameters) <- paste0("parameters.",names(parameters))
    mcmc_derive(aggregate.list, expr.all.params, parameters, silent=TRUE) %>%
      return
  }else{return(aggregate.list)}
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

prepare_code <- function(code, code.add, code.values){
  code <- paste(c(code, code.add), 
                collapse=" \n", 
                sep="")
  if(str_detect(code, "^\\s*(data)|(model)\\s*[{]"))
    err("jags code must not be in a data or model block")
  
  do.call(sprintf, args = as.list(c(code, code.values))) %>% 
    return
}

fun.batchr.analyse <- function(file, path.save, ...){#, code, n.adapt, n.burnin, n.iter, monitor){
  readRDS(file) %>%
    sma_analyse_bayesian(...) %>%
    saveRDS(file.path(path.save, sub("data", "analys", basename(file))))
}

analyse_to_file <- function(path, ...){
  
  saving.dir <- file.path(path, "analysis") 
  if(!dir.exists(saving.dir)) dir.create(saving.dir)
  batch_process(fun.batchr.analyse, 
                path=path,
                regexp="^data\\d{7,7}.rds$", 
                path.save=file.path(path, "analysis"),
                ask=FALSE,
                ...)
}

# 
# extract_measure_from_summary <- function(summaries, word, monitor){
#   indices <- summaries %>% 
#     names %>%
#     startsWith(p0(word,".")) %>% 
#     which()
#   measure_res = summaries[indices]
#   #monitor= measure_res %>% 
#   #  names %>% 
#   #  sub(pattern='.*\\.', replacement='')
#   names(measure_res)=monitor
#   return(measure_res)
# }
# 
# summary_reformat <- function(summaries, measures, monitor){
#   measures %>% 
#     lapply(extract_measure_from_summary, 
#            summaries=summaries,
#            monitor=monitor) %>%
#     set_names(measures)
# }
# 
