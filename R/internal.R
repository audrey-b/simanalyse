add_model_block <- function(code){
  
  sprintf_custom("model{\n\n?\n\n}", code)
  
}


set_seed_inits <- function(inits, n.chains, nlistdata) {
  
  if(class(inits)=="function"){
    names_arguments <- names(as.list(args(inits)))
    names_arguments <- names_arguments[-length(names_arguments)]
    if(!is.null(names_arguments)){
      chk_subset(names_arguments, c("chain",  "data"))
      if(length(names_arguments) == 1){
        chk_equal(names_arguments, "chain")
        inits = lapply(1:n.chains, inits)
      }else{
        inits = lapply(1:n.chains, inits, data = nlistdata)
      }
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
    
    n.lists <- length(inits)
    stream <- .Random.seed
    load.module("lecuyer")
    # list.factories(type="rng")
    
    for(i in 1:n.lists){
      inits[[i]]$.RNG.name <- "lecuyer::RngStream"
      inits[[i]]$.RNG.state <- stream[-1]
      stream <- parallel::nextRNGSubStream(stream)
    }  
    
    
    #RNGversion(getRversion())
    
  }
  
  inits
  
}

analyse_dataset_bayesian <- function(nlistdata, code, monitor, deviance, 
                                     n.chains=3, inits=list(), n.adapt, 
                                     n.save, max.time, max.iter,
                                     quiet = FALSE, units="mins", 
                                     ess, r.hat, ess.nodes, r.hat.nodes,
                                     normalize) {
  
  if(deviance == TRUE){
    load.module("dic")
    monitor <- unique(c(monitor, "deviance"))
  }
  
  code %<>% add_model_block() %>% textConnection
  
  inits <- set_seed_inits(inits, n.chains, nlistdata)
  
  #adaptation
  model <- rjags::jags.model(code, data = nlistdata, inits = inits,
                             
                             n.adapt = n.adapt, n.chains = n.chains, quiet = quiet)
  
  #if(n.burnin >= 1) update(model, n.iter = n.burnin)
  time0 <- Sys.time()
  thin=1
  
  if(".*" %in% monitor){
    variable.names = names(model$state()[[1]])
    if("deviance" %in% monitor){
      variable.names = c(variable.names, "deviance")
    }
  }else{variable.names=monitor}
  
  sample <- rjags::jags.samples(model, 
                                variable.names = variable.names, 
                                n.iter = n.save) %>%
    mcmcr::as.mcmcr()
  
  cum.iter=n.save
  batch.time = as.double(difftime(Sys.time(), time0, units=units))
  cum.time = batch.time
  
  if(".*" %in% r.hat.nodes) r.hat.nodes <- pars(sample)
  if(".*" %in% ess.nodes) ess.nodes <- pars(sample)
  
  sample.norm <- sample
  
  if(normalize == TRUE){
    
    which.log <- which(lapply(1:length(sample), function(i){(sum(sample[[i]] < 0) == 0) & (sum(sample[[i]] > 1) > 0)}) == TRUE)
    which.logit <- which(lapply(1:length(sample), function(i){(sum(sample[[i]] < 0) == 0) & (sum(sample[[i]] > 1) == 0)}) == TRUE)
    if(length(which.log) != 0) sample.norm[which.log] <- lapply(sample.norm[which.log], log)
    if(length(which.logit) != 0) sample.norm[which.logit] <- lapply(sample.norm[which.logit], logit)
    
  }
  
  r.hat.convergence <- max(mcmcr::rhat(subset(sample.norm, pars=r.hat.nodes), as_df=TRUE, by="term")$rhat, na.rm=TRUE)
  ess.convergence <- min(mcmcr::ess(subset(sample.norm, pars=ess.nodes), as_df=TRUE, by="term")$ess, na.rm=TRUE)
  
  cat(paste0("\nMax r.hat= ", r.hat.convergence, "\n"))
  cat(paste0("Min ess= ", ess.convergence, "\n"))
  
  r.hat.convergence <- r.hat.convergence<=r.hat
  ess.convergence <- ess.convergence>=ess
  convergence <- (r.hat.convergence & ess.convergence) 
  
  while(cum.time+batch.time*2 <= max.time & cum.iter+n.save*(thin+1) <= max.iter & !convergence){
    thin=thin+1
    sample <- rjags::jags.samples(model, 
                                  variable.names = variable.names, 
                                  n.iter = n.save*thin, thin=thin) %>%
      mcmcr::as.mcmcr()
    
    cum.iter = cum.iter+n.save*thin
    batch.time = as.double(difftime(Sys.time(), time0, units=units))-cum.time
    cum.time = cum.time + batch.time
    
    sample.norm <- sample
    
    if(normalize == TRUE){
      
      which.log <- which(lapply(1:length(sample), function(i){(sum(sample[[i]] < 0) == 0) & (sum(sample[[i]] > 1) > 0)}) == TRUE)
      which.logit <- which(lapply(1:length(sample), function(i){(sum(sample[[i]] < 0) == 0) & (sum(sample[[i]] > 1) == 0)}) == TRUE)
      if(length(which.log) != 0) sample.norm[which.log] <- lapply(sample.norm[which.log], log)
      if(length(which.logit) != 0) sample.norm[which.logit] <- lapply(sample.norm[which.logit], logit)
      
    }
    
    r.hat.convergence <- max(mcmcr::rhat(subset(sample.norm, pars=r.hat.nodes), as_df=TRUE, by="term")$rhat, na.rm=TRUE)
    ess.convergence <- min(mcmcr::ess(subset(sample.norm, pars=ess.nodes), as_df=TRUE, by="term")$ess, na.rm=TRUE)
    
    cat(paste0("\nMax r.hat= ", r.hat.convergence, "\n"))
    cat(paste0("Min ess= ", ess.convergence, "\n"))
    
    r.hat.convergence <- r.hat.convergence<=r.hat
    ess.convergence <- ess.convergence>=ess
    convergence <- (r.hat.convergence & ess.convergence)  
  }
  
  if(deviance == TRUE) unload.module("dic")
  
  return(mcmcr::as.mcmcr(sample))
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
                               h_null,
                               custom_FUNS = NULL,
                               custom_expr_before = "", 
                               custom_expr_after = ""){
  expr <- NULL
  aggregate.FUNS <- NULL
  derive_expr <- NULL
  
  #aggregate_FUNS
  if(sum(c("E", "bias", "mse", "rb", "br", "cv", "var", "se", "rmse", "rrmse", "all") %in% measures) > 0) {
    aggregate.FUNS %<>% append(list(estimator = estimator))
  }
  if(sum(c("Epvar", "all") %in% measures) > 0){
    aggregate.FUNS %<>% append(list(pvar = var))
  }
  if(sum(c("Epsd", "all") %in% measures) > 0){
    aggregate.FUNS %<>% append(list(psd = sd))
  }
  if(sum(c("cpQuantile", "LQuantile", "power", "all") %in% measures) > 0){
    cp.lower.with.alpha = function(x) do.call("cp.lower",list(x,"alpha"=alpha))
    cp.upper.with.alpha = function(x) do.call("cp.upper",list(x,"alpha"=alpha))
    aggregate.FUNS %<>% append(list(cp.lower = cp.lower.with.alpha, 
                                    cp.upper = cp.upper.with.alpha))
  }
  
  expr <- derive_expr(c("bias", "rb", "br"), "bias = estimator - parameters", measures, expr)
  expr <- derive_expr(c("mse", "rmse", "rrmse"), "mse = (estimator - parameters)^2", measures, expr)
  expr <- derive_expr(c("cpQuantile"), "cpQuantile = ifelse((parameters >= cp.lower) & (parameters <= cp.upper), 1, 0)", measures, expr)
  expr <- derive_expr(c("power"), paste0("power = 1-ifelse((", h_null, " >= cp.lower) & (", h_null, " <= cp.upper), 1, 0)"), measures, expr)
  expr <- derive_expr(c("E", "cv", "var", "se", "br"), "E = estimator", measures, expr)
  expr <- derive_expr(c("cv", "var", "se", "br"), "Esq = estimator^2", measures, expr)
  expr <- derive_expr(c("LQuantile"), "LQuantile = cp.upper - cp.lower", measures, expr)
  expr <- derive_expr(c("Epvar"), "Epvar = pvar", measures, expr)
  expr <- derive_expr(c("Epsd"), "Epsd = psd", measures, expr)
  derive_expr <- derive_expr(c("rb"), "rb = bias/parameters", measures, derive_expr)
  derive_expr <- derive_expr(c("var", "br", "se", "cv"), "var = Esq - E^2", measures, derive_expr)
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
                                  options = furrr::furrr_options()){
  
  future_res <- future_map(listnlists, evaluate_within, 
                           expr = expr_FUNS[["expr"]], 
                           aggregate.FUNS = expr_FUNS[["aggregate.FUNS"]], 
                           parameters = parameters, 
                           .progress = progress, 
                           .options=options) 
  
  future::resetWorkers(future::plan())
  
  future_res %>%
    as_nlists() %>%
    evaluate_across(mean) %>%
    derive_measures(expr_FUNS[["derive_expr"]], 
                    measure_names(expr_FUNS[["expr"]]), 
                    parameters) %>%
    return()
}


evaluate_all_measures_files <- function(files, 
                                        expr_FUNS, 
                                        parameters,
                                        progress = FALSE,
                                        options = furrr::furrr_options(),
                                        monitor,
                                        deviance=deviance,
                                        measures=measures){
  
  future_res <- future_map(files, 
                           function(file, expr, aggregate.FUNS, parameters){
                             object = mcmcr::as.mcmcrs(readRDS(file))
                             object %<>% lapply(function(x) mcmcr::collapse_chains(x)) %<>% (mcmcr::as.mcmcrs)
                             mcmcr::chk_mcmcrs(object)
                             if((".*" %in% monitor) & (deviance==FALSE)){
                               monitor = pars(object[[1]])
                               monitor = monitor[monitor!="deviance"]
                             }
                             if(!(".*" %in% monitor)){object %<>% lapply(subset, pars=monitor)
                               #parameters %<>% parameters[monitor]
                             }else{monitor = pars(object[[1]])}
                             
                             monitor.with.params <- names(parameters)[monitor %in% names(parameters)]
                             nas_measures <- c("bias", "cpQuantile", "rb", "br", "mse", "rmse", "rrmse", "all")
                             problem_measures <- nas_measures[nas_measures %in% measures]
                             if(!vld_equal(sort(monitor.with.params), sort(monitor)) && length(problem_measures)>0){
                               err("True parameter values are missing for requested measures ", problem_measures)     
                             } 
                             
                             evaluate_within(object[[1]], expr, aggregate.FUNS, parameters)}, 
                           expr = expr_FUNS[["expr"]], 
                           aggregate.FUNS = expr_FUNS[["aggregate.FUNS"]], 
                           parameters = parameters, .progress = progress, .options=options) 
  
  future_res <- as_nlists(future_res)
  
  future::resetWorkers(future::plan())
  
  future_res %>%
    as_nlists() %>%
    evaluate_across(mean) %>%
    derive_measures(expr_FUNS[["derive_expr"]], 
                    measure_names(expr_FUNS[["expr"]]), 
                    parameters) %>%
    return()
}

derive_measures <- function(nlist, derive_expr, keywords, parameters){
  if(!is.null(derive_expr)){
    monitor = names(parameters)
    expr.all.params <- expand_expr(derive_expr, keywords, monitor, parameters)
    names(parameters) <- paste0("parameters.",names(parameters))
    mcmc_derive(object = nlist, expr = expr.all.params, values = parameters, silent = TRUE) %>%
      append(nlist) %>%
      return()
  }else{return(nlist)}
}

evaluate_across <- function(summary.nlist, FUN){
  summary.nlist %>% 
    estimates(fun = mean) %>%
    return()
}

expand_expr <- function(expr, keywords, monitor, parameters){
  measures <- measure_names(expr)
  keywords %<>% c(measures, "parameters")
  expr.all.params <- monitor %>% 
    lapply(make_one_expr, expr, keywords) %>%
    unlist %>%
    paste(collapse=" \n ") %>%
    return()
}

summarise_one_result <- function(nlists, aggregate.FUNS){
  aggregate.FUNS %>% 
    lapply(function(FUN, nlists) estimates(nlists, FUN), nlists)
}

evaluate_within <- function(nlists, expr, aggregate.FUNS, parameters, ...){
  
  # if statement added because as_nlists was removed from sma_evaluate()
  # It was removed because it made the function very slow
  if(class(nlists) == "nlists") {monitor = nlists[[1]] %>% names()}
  else {monitor = nlists %>% names()}
  #calculate aggregates
  
  aggregate.list <- nlists %>% 
    summarise_one_result(aggregate.FUNS)%>%
    unlist(recursive=FALSE) %>% #now names are of the form estimator.mu
    as_nlist()
  
  #apply expr
  if(!is.null(expr)){
    
    expr.all.params <- expand_expr(expr, names(aggregate.FUNS), monitor, parameters)
    names(parameters) <- paste0("parameters.",names(parameters))
    
    mcmc_derive(object = aggregate.list, expr = expr.all.params, values = parameters, silent=TRUE) %>%
      return()
    
  }else{return(aggregate.list)}
}

make_one_expr <- function(param, expr, keywords){
  patterns <- str_c("\\b", keywords, "\\b", collapse="|")
  str_replace_all(expr, 
                  patterns, 
                  function(x) if(x %in% keywords) return(chk::p0(x,".",param)))
}

strip_comments <- function(x) {
  str_replace_all(x, pattern = "\\s*#[^\\\n]*", replacement = "")
}

measure_names <- function (x) {
  x <- strip_comments(x)
  
  pattern <- "(?=\\s*([=]|([<][-])))"
  
  index <- "\\[[^\\]]*\\]"
  
  pattern <- chk::p0("\\w+(", index, "){0,1}\\s*[)]{0,1}", pattern, collapse = "")
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
  
  if(!is.null(code.values)) code <- sprintf_custom(code, code.values)
  
  return(code)
}

fun.batchr <- function(file, path.save, sma.fun, suffix, ...){#, code, n.adapt, n.burnin, n.iter, monitor){
  base.name <- basename(file)
  id <- str_extract(base.name, "[[:digit:]]+") %>% as.integer()
  prefix <- str_split(base.name, "[[:digit:]]+")[[1]][1] #kinda hacky, could have only one line of code for id and prefix
  readRDS(file) %>%
    sma.fun(...) %>%
    saveRDS(file.path(path.save, sub(prefix, suffix, base.name)))
}

sma_batchr <- function(sma.fun, prefix, suffix, path.read, 
                       path.save, folder,
                       options,
                       seeds=NULL,
                       ...){
  if(!dir.exists(path.save)) dir.create(path.save, recursive=TRUE)
  batch_config(fun=fun.batchr, 
               path=path.read, 
               regexp = chk::p0("^", prefix, "\\d{7,7}.rds$"),
               path.save = path.save,
               sma.fun = sma.fun,
               suffix = suffix,
               ...)
  batch_run(
    path=path.read,
    seeds = seeds,
    options = options,
    ask = FALSE  )
  
  # batch_process(fun = fun.batchr, 
  #               path = path.read,
  #               regexp = chk::p0("^", prefix, "\\d{7,7}.rds$"), 
  #               ask = FALSE,
  #               options = options,
  #               seeds = seeds,
  #               path.save = path.save,
  #               sma.fun = sma.fun,
  #               suffix = suffix,
  #               ...)
  #...
  # code=code, monitor=monitor,
  # inits=inits, n.chains=mode$n.chains,
  # n.adapt=mode$n.adapt, max.time=mode$max.time,
  # max.iter=mode$max.iter, n.save=mode$n.save, 
  # ess=mode$ess, r.hat=mode$r.hat,
  # units=mode$units
}

derive_one <- function(object.nlists, code){
  derived_obj <- object.nlists %>% 
    mcmc_derive(expr = code, 
                primary = TRUE, 
                silent = TRUE) %>%
    return()
  #if(monitor != ".*") return(subset(derived_obj, select=monitor))
  #return(derived_obj)
}

sma_derive_internal <- function(object, code, monitor, values, monitor.non.primary, progress, options){
  if(class(object)=="mcmcrs"){ #| class(object)=="nlists"){ add if want to allow sma_derive on data
    new_obj <- future_map(object, mcmc_derive, expr=code, monitor=monitor.non.primary, values=values, primary=TRUE, .progress=progress, .options=options, silent = TRUE)
    if(!(".*" %in% monitor)) new_obj <- future_map(new_obj, subset, pars=monitor, .progress=progress, .options=options) #remove primary that are not in monitor
  }else if(class(object)=="mcmcr" | class(object)=="nlist"){
    new_obj <- mcmc_derive(object, code, monitor=monitor.non.primary, values=values, primary=TRUE, silent=TRUE)    
    if(!(".*" %in% monitor)) new_obj <- subset(new_obj, pars=monitor) #remove primary that are not in monitor
    if(class(object)=="nlist") new_obj <- as_nlist(new_obj)
  }
  
  return(new_obj)
}

# sma_autojags <- function(nlistdata, code, monitor, n.chains=3, 
#                          inits, n.adapt=1000, 
#                          max.iter=200, max.time=0.1, max.save=500,
#                          batch=200,  n.burnin=0){
#   
#   #thin=ifelse(n.batch>max.save, n.batch/max.save, 1) #need to round?yes!
#   chains=NULL
#   cum.iter=0; cum.time=0; cum.saved=0
#   thin.new=1
#   time0 <- Sys.time()
#   #while(not_converged(old) & cum.time < max.time & cum.iter < max.iter){
#   while(cum.time < max.time & cum.iter < max.iter){
#     if(cum.saved + ceiling(n.batch/thin.new) > max.save){
#       if(!is.null(old)) old <- thin(old, 2)
#       thin.new=thin.new*2}
#     new <- analyse_dataset_bayesian(nlistdata,
#                                     code,
#                                     monitor,
#                                     n.chains=3,
#                                     inits,
#                                     n.adapt=n.adapt,
#                                     n.burnin=n.burnin,
#                                     n.iter=batch,
#                                     thin=thin.new,
#                                     quiet = FALSE)
#     chains <- put_together(chains, new)
#     cum.saved=niters(chains)
#     cum.iter=cum.saved*thin.new
#     cum.time=as.double(difftime(Sys.time(),time0, units="mins"))
#   }
# }
