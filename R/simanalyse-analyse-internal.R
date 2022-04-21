sma_analyse_internal <- function(sims = NULL,
                        code,
                        code.add="",
                        code.values=NULL,
                        monitor = ".*",
                        inits=list(),
                        mode=sma_set_mode("report"),
                        deviance = TRUE,
                        #pD = FALSE,
                        #save= NA,
                        path = ".",
                        folder = "analysis0000001",
                        progress = FALSE,
                        options = furrr::furrr_options(seed = TRUE)){
  
  if(!is.null(sims)){
    if(is.list(sims) && !is_nlist(sims) && !is_nlists(sims) && length(lengths(sims))==1){
      class(sims) <- "nlist"
      chk_nlist(sims)
      sims <- nlists(sims)}
    if(is.list(sims) && !is_nlist(sims) && !is_nlists(sims) && length(lengths(sims))>1){
      class(sims) <- "nlists"
      for(i in 1:length(sims)) class(sims[[i]]) <- "nlist"
      
      chk_nlists(sims)   } 
    n.sims <- length(sims)  
  }else{
    chk_string(path)
    n.sims <- length(sims_data_files(path))
  }
  
  
  chk_string(code)
  chk_string(code.add)
  if(!is.null(code.values)) chk_all(code.values, chk_string)
  chk_character(monitor)
  chkor(chk_list(inits), chk_function(inits))
  #lapply(chk_) need to figure out
  chk_list(mode)
  #chk_lgl(save)
  
  #need to check that r.hat.node and ess.nodes are contained within monitor
  
  chk_flag(progress)
  chk_s3_class(options, "furrr_options")
  
  
  if(!is.list(options$seed)){ #error if list not the right length
    plan_current <- future::plan()
    future::plan("sequential")
    seeds <- furrr::future_map(1:n.sims, 
                               function(x) return(.Random.seed), 
                               .options = options)
    names(seeds) = chk::p0("data", sprintf("%07d", 1:n.sims), ".rds")
    options$seed = seeds
    future::plan(plan_current)
  }
  
  if(is.null(sims)){
    if(!dir.exists(file.path(path, folder))) dir.create(file.path(path, folder))
    saveRDS(seeds, file.path(path, folder, ".seeds.rds"))
  }
  
  res.list <- list(nlists(nlist()))
  
  code %<>% prepare_code(code.add, code.values)
  

  #if(pD == TRUE) monitor <- unique(c(monitor, "pD"))
  
  #jags
  if(!is.null(sims)){
    #if(!is.null(path) & is.null(sims)) sims <- sims_data(path)
    
    res.list <- future_pmap(list(nlistdata=sims), analyse_dataset_bayesian, 
                            code=code, monitor=monitor, deviance = deviance,
                            inits=inits, n.chains=mode$n.chains,
                            n.adapt=mode$n.adapt, max.time=mode$max.time,
                            max.iter=mode$max.iter, n.save=mode$n.save, 
                            ess=mode$ess, r.hat=mode$r.hat,
                            ess.nodes=mode$ess.nodes, 
                            r.hat.nodes=mode$r.hat.nodes,
                            normalize = mode$normalize,
                            units=mode$units, .progress = progress, .options=options)
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    return((mcmcr::as.mcmcrs(res.list)))
    
  }else{
    options$seed = FALSE
    sma_batchr(sma.fun = analyse_dataset_bayesian,
               path.read = path,
               folder = folder,
               path.save = file.path(path, folder, "results"),
               prefix = "data", suffix = "results",
               code = code, monitor = monitor, deviance = deviance,
               inits = inits, n.chains = mode$n.chains,
               n.adapt = mode$n.adapt, max.time = mode$max.time,
               max.iter = mode$max.iter, n.save = mode$n.save, 
               ess = mode$ess, r.hat = mode$r.hat,
               ess.nodes = mode$ess.nodes, 
               r.hat.nodes = mode$r.hat.nodes,
               normalize = mode$normalize,
               units = mode$units, options = options, seeds = seeds)
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
  }
  
  future::resetWorkers(future::plan())
}