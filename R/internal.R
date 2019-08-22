add_model_block <- function(code){
  
  sprintf("model{\n\n%s\n\n}", code)
  
}


set_seed_inits <- function(seed, inits, n.chains) {
  
  set.seed(seed)
  
  if(class(inits)=="function"){
    if(!is.null(names(as.list(args(inits)))[1])){
      inits = lapply(1:n.chains, inits)
    }else{
      inits = lapply(1:n.chains, function(x) inits())
    }
  }
  
  if(length(inits)==0){ #default inits
    
    inits$.RNG.name <- "base::Wichmann-Hill"
    inits$.RNG.seed <- abs(as.integer(rinteger()))
    
  }else if(!chk_list(inits[[1]], err=FALSE)){ #1 set of inits, same for each chain
    
    inits$.RNG.name <- "base::Wichmann-Hill"
    inits$.RNG.seed <- abs(as.integer(rinteger()))
    
  }else{ #more than 1 set of inits
    
    set.seed(seed, "L'Ecuyer-CMRG")
    
    n.lists <- length(inits)
    stream <- .Random.seed
    load.module("lecuyer")
    # list.factories(type="rng")
    
    for(i in 1:n.lists){
      inits[[i]]$.RNG.name <- "lecuyer::RngStream"
      inits[[i]]$.RNG.state <- stream[-1]
      stream <- nextRNGStream(stream)
    }  
    
    
    RNGversion(getRversion())

  }
  
  inits
  
}

analyse_dataset_bayesian <- function(nlistdata, code, monitor, 
                                     n.chains=3, inits=list(), n.adapt, n.burnin, 
                                     n.iter, thin=1, seed = rinteger(), 
                                     quiet = FALSE) {
  
  code %<>% add_model_block() %>% textConnection
  
  inits <- set_seed_inits(seed, inits, n.chains)
  
  model <- rjags::jags.model(code, data = nlistdata, inits = inits,
                             
                             n.adapt = n.adapt, n.chains = n.chains, quiet = quiet)
  
  if(n.burnin >= 1) update(model, n.iter = n.burnin)
  
  sample <- rjags::jags.samples(model, variable.names = monitor, n.iter = n.iter, thin=thin)
  
  #nlists <-  sample %>% as.mcmcr() %>% collapse_chains() %>% as.nlists
  
  return(as.mcmcr(sample))
  #saveRDS(nlist, file.path(path, data_file_name(sim)))
  #data_file_name <- function(sim) p0("data", sprintf("%07d", sim), ".rds")
  
}

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
    cp.lower.with.alpha = function(x) do.call("cp.lower",list(x,"alpha"=alpha))
    cp.upper.with.alpha = function(x) do.call("cp.upper",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list(cp.lower = cp.lower.with.alpha, cp.upper = cp.upper.with.alpha))
  }
  
  #expr
  if(sum(c("bias", "rb", "br", "var", "se", "all") %in% measures) > 0){
    expr=paste(c(expr, "bias = estimator - parameters"), collapse=" \n ", sep="")
  }
  if(sum(c("mse", "br", "var", "se", "rmse", "rrmse", "all") %in% measures) > 0){
    expr=paste(c(expr, "mse = (estimator - parameters)^2"), collapse=" \n ", sep="")
  }
  if(sum(c("cp.quantile", "all") %in% measures) > 0){
    expr=paste(c(expr, "cp.quantile = ifelse((parameters >= cp.lower) & (parameters <= cp.upper), 1, 0)"), collapse=" \n ", sep="")
  }
  if(sum(c("E", "cv", "all") %in% measures) > 0){
    expr=paste(c(expr, "E = estimator"), collapse=" \n ", sep="")
  }
  if(sum(c("cp.length", "all") %in% measures) > 0){
    expr=paste(c(expr, "cp.length = cp.upper - cp.lower"), collapse=" \n ", sep="")
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

assess_all_measures <- function(listnlists, 
                                expr_FUNS, 
                                parameters){
  
  listnlists %>% 
    lapply(assess_within, expr_FUNS[["expr"]], 
           expr_FUNS[["aggregate.FUNS"]], 
           parameters) %>%
    as.nlists() %>%
    assess_across(mean) %>%
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
    mcmc_derive(object = nlist, expr = expr.all.params, values = parameters, silent = TRUE) %>%
      append(nlist) %>%
      return
  }else{return(nlist)}
}

assess_across <- function(summary.nlist, FUN){
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

summarise_one_result <- function(nlists, aggregate.FUNS){
  aggregate.FUNS %>% 
    lapply(function(FUN, nlists) aggregate(nlists, FUN), nlists)
}

assess_within <- function(nlists, expr, aggregate.FUNS, parameters){
  monitor = nlists[[1]] %>% names()
  #calculate aggregates
  aggregate.list <- nlists %>% 
    summarise_one_result(aggregate.FUNS)%>%
    unlist(recursive=FALSE) %>% #now names are of the form estimator.mu
    as.nlist()
  #apply expr
  if(!is.null(expr)){
    expr.all.params <- expand_expr(expr, names(aggregate.FUNS), monitor, parameters)
    names(parameters) <- paste0("parameters.",names(parameters))
    mcmc_derive(object = aggregate.list, expr = expr.all.params, values = parameters, silent=TRUE) %>%
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

fun.batchr <- function(file, path.save, seeds, sma.fun, suffix, ...){#, code, n.adapt, n.burnin, n.iter, monitor){
  base.name <- basename(file)
  id <- str_extract(base.name, "[[:digit:]]+") %>% as.integer()
  prefix <- str_split(base.name, "[[:digit:]]+")[[1]][1] #kinda hacky, could have only one line of code for id and prefix
  readRDS(file) %>%
    sma.fun(seed=seeds[id], ...) %>%
    saveRDS(file.path(path.save, sub(prefix, suffix, base.name)))
}

sma_batchr <- function(sma.fun, prefix, suffix, path.read, path.save, seeds, ...){
  
  if(!dir.exists(path.save)) dir.create(path.save)
  batch_process(fun = fun.batchr, 
                path=path.read,
                regexp=p0("^", prefix, "\\d{7,7}.rds$"), 
                ask=FALSE,
                path.save = path.save,
                seeds = seeds,
                sma.fun = sma.fun,
                suffix = suffix,
                ...)
}

derive_one <- function(object.nlists, code){
  derived_obj <- object.nlists %>% 
    mcmc_derive(expr = code, 
                primary = TRUE, 
                silent = TRUE) %>%
    return
  #if(monitor != ".*") return(subset(derived_obj, select=monitor))
  #return(derived_obj)
}
 
