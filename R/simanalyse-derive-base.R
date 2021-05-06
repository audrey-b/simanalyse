sma_derive_base <- function(object=NULL, code, monitor=".*", 
                       values=list(),
                       path = ".",
                       folder = "analysis0000001",
                       progress = FALSE,
                       options = furrr::furrr_options()) {
  
  
  if(class(object) == "list") object <- as_nlist(object)
  if(!mcmcr::is.mcmcr(object) & !is_nlist(object) & length(lengths(object))==1){
    object <- mcmcr::as.mcmcr(object)
    mcmcr::chk_mcmcr(object)}
  if(!is_nlists(object) & !mcmcr::is.mcmcrs(object) & length(lengths(object))>1){
    object <- mcmcr::as.mcmcrs(object)
    mcmcr::chk_mcmcrs(object)
  }
  # chk_nlists(object)
  # 
  # if(!is_nlist(object) && !is_nlists(object) && length(lengths(object))==1){
  #   class(object) <- "nlist"
  #   chk_nlist(object)
  #   object <- nlists(object)}
  # if(!is_nlist(object) && !is_nlists(object) && length(lengths(object))>1){
  #   class(object) <- "nlists"
  #   for(i in 1:length(object)) class(object[[i]]) <- "nlist"
  # }
  # chk_nlists(object)
  # 
  # 
  #do not monitor non-primary variables that are not in monitor
  monitor.non.primary <- ".*" 
  if(!(".*" %in% monitor)){
    if(class(object) %in% c("nlist")){
      primary.params <- names(object)
    }else{
      primary.params <- names(object[[1]])
    }
    
    monitor.non.primary <- monitor[!(monitor %in% primary.params)]
  }
  
  
  
  #if(length(monitor.non.primary) > 1) monitor <- paste(monitor.non.primary, collapse=" | ") #make regular expression
  
  if(!is.null(object)){
    #files <- list.files(path, pattern = "^results\\d{7,7}[.]rds$")
    #object <- lapply(file.path(path, files), readRDS)
    sma_derive_internal(object, code, monitor, values, monitor.non.primary, progress, options)
    
  }else{
    chk_dir(path)
    
    sma_batchr(sma.fun=sma_derive_internal, 
               folder=folder,
               path.read = file.path(path, folder, "results"),
               path.save = file.path(path, folder, "derived"),
               prefix="results", suffix="deriv",
               code=code, monitor=monitor, values=values,
               monitor.non.primary=monitor.non.primary,
               progress=progress, options=options) #need to change
    
    parameters <- sims_info(path)$parameters
    derived.params <- sma_derive_internal(parameters, code, monitor, values, monitor.non.primary, progress, options)
    saveRDS(derived.params, file.path(path, folder, "derived", ".parameters.rds"))
  }
}
