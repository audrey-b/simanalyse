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
    
  }else if(!vld_list(inits[[1]])){ #1 set of inits, same for each chain

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
      stream <- parallel::nextRNGStream(stream)
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
  
  return((mcmcr::as.mcmcr(sample)))
  #saveRDS(nlist, file.path(path, data_file_name(sim)))
  #data_file_name <- function(sim) p0("data", sprintf("%07d", sim), ".rds")
  
}

derive_expr <- function(names, add_expr, measures, expr, all="all"){
  if(sum(c(names, "all") %in% measures) > 0){
    expr = paste(c(expr, add_expr), collapse=" \n ", sep="")
  }
  return(expr)
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
    aggregate.FUNS %<>% append(list(cp.lower = cp.lower.with.alpha, 
                                    cp.upper = cp.upper.with.alpha))
  }
  
  expr <- derive_expr(c("bias", "rb", "br", "var", "se"), "bias = estimator - parameters", measures, expr)
  expr <- derive_expr(c("mse", "br", "var", "se", "rmse", "rrmse"), "mse = (estimator - parameters)^2", measures, expr)
  expr <- derive_expr(c("cp.quantile"), "cp.quantile = ifelse((parameters >= cp.lower) & (parameters <= cp.upper), 1, 0)", measures, expr)
  expr <- derive_expr(c("E", "cv"), "E = estimator", measures, expr)
  expr <- derive_expr(c("cp.length"), "cp.length = cp.upper - cp.lower", measures, expr)
  derive_expr <- derive_expr(c("rb"), "rb = bias/parameters", measures, derive_expr)
  derive_expr <- derive_expr(c("var", "br", "se", "cv"), "var = mse - bias^2", measures, derive_expr)
  derive_expr <- derive_expr(c("br"), "br = bias/sqrt(var)", measures, derive_expr)
  derive_expr <- derive_expr(c("se"), "se = sqrt(var)", measures, derive_expr)
  derive_expr <- derive_expr(c("cv"), "cv = sqrt(var)/E", measures, derive_expr)
  derive_expr <- derive_expr(c("rmse"), "rmse = sqrt(mse)", measures, derive_expr)
  derive_expr <- derive_expr(c("rrmse"), "rrmse = sqrt(mse)/parameters", measures, derive_expr)
  
  if(custom_expr_before!="") expr=paste(c(expr, custom_expr_before), collapse=" \n ", sep="")
  if(custom_expr_after!="") derive_expr=paste(c(derive_expr, custom_expr_after), collapse=" \n ", sep="")
  if(!is.null(custom_FUNS)) aggregate.FUNS %<>% append(custom_FUNS)
  
  return(list(expr=expr, aggregate.FUNS=aggregate.FUNS, derive_expr=derive_expr))
}

evaluate_all_measures <- function(listnlists, 
                                expr_FUNS, 
                                parameters,
                                progress = FALSE,
                                options = furrr::future_options()){
  
  future_map(listnlists, evaluate_within, 
             expr = expr_FUNS[["expr"]], 
             aggregate.FUNS = expr_FUNS[["aggregate.FUNS"]], 
             parameters = parameters, .progress = progress, .options=options) %>%
    as.nlists() %>%
    evaluate_across(mean) %>%
    derive_measures(expr_FUNS[["derive_expr"]], 
                    measure_names(expr_FUNS[["expr"]]), 
                    parameters) %>%
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

evaluate_across <- function(summary.nlist, FUN){
  summary.nlist %>% 
    aggregate(fun = mean) %>%
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

evaluate_within <- function(nlists, expr, aggregate.FUNS, parameters){
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
  
  if(!dir.exists(path.save)) dir.create(path.save, recursive=TRUE)
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

sma_derive_internal <- function(object, code, monitor, monitor.non.primary, progress, options, seed){
  #seed ?? do something with it?
  seed <- seed
if(class(object)=="mcmcrs"){
  new_obj <- future_map(object, mcmc_derive, expr=code, monitor=monitor.non.primary, primary=TRUE, .progress=progress, .options=options)
  if(monitor!=".*") new_obj <- future_map(new_obj, subset, pars=monitor, .progress=progress, .options=options) #remove primary that are not in monitor
}else if(class(object)=="mcmcr" | class(object)=="nlist"){
  new_obj <- mcmc_derive(object, code, monitor=monitor.non.primary, primary=TRUE)    
  if(monitor!=".*") new_obj <- subset(new_obj, pars=monitor) #remove primary that are not in monitor
}
return(new_obj)
}
