sma_evaluate_internal <- function(object = NULL, 
                         measures=c("bias", "mse", "cpQuantile"), 
                         estimator=mean, 
                         alpha=0.05,
                         parameters = NULL,
                         monitor=".*",
                         deviance=FALSE,
                         path = ".",
                         folder = "analysis0000001",
                         custom_funs = list(),
                         custom_expr_before="",
                         custom_expr_after="",
                         progress = FALSE,
                         options = furrr::furrr_options()){
  
  chk_vector(measures); chk_all(measures, "chk_string")
  chk_function(estimator)
  chk_vector(monitor); chk_all(monitor, chk_string)
  chk_number(alpha); chk_range(alpha, c(0,1))
  chk_string(custom_expr_before)
  chk_string(custom_expr_after)
  chk_flag(progress)
  chk_flag(deviance)
  chk_s3_class(options, "furrr_options")
  
  chk_list(custom_funs)
  if(!length(custom_funs)==0){
    chk_named(custom_funs)
    lapply(custom_funs, chk_function)
  }else custom_funs=NULL
  
  read.file=is.null(object)
  
  if(read.file){
    chk_dir(path)
    derive.path <- file.path(path, folder, "derived")
    if(dir.exists(derive.path)){
      prefix = "deriv"
      if(is.null(parameters)) parameters = readRDS(list.files(path=derive.path, pattern=".parameters.rds", all.files=TRUE, full.names=TRUE))
    }else{                        
      prefix="results"
      if(is.null(parameters)) parameters = sims_info(path)$parameters
    }
    
    if(is.list(parameters) && !is_nlist(parameters)) class(parameters) <- "nlist"
    chk_nlist(parameters)
    
    files <- list.files(path=file.path(path, folder), pattern=chk::p0("^", prefix, "\\d{7,7}.rds$"), recursive=TRUE, full.names=TRUE)
    #object <- mcmcr::as.mcmcrs(lapply(files, readRDS))
    performance <- evaluate_all_measures_files(files, 
                                               make_expr_and_FUNS(measures, parameters, estimator, alpha, custom_funs, custom_expr_before, custom_expr_after), 
                                               parameters,
                                               progress=progress,
                                               options=options,
                                               monitor=monitor,
                                               deviance=deviance,
                                               measures=measures)
  }else{
    if(is.list(parameters) && !is_nlist(parameters)) 
      class(parameters) <- "nlist"
    chk_nlist(parameters)
    object %<>% lapply(function(x) mcmcr::collapse_chains(x)) %<>% (mcmcr::as.mcmcrs)
    mcmcr::chk_mcmcrs(object)
    if((".*" %in% monitor) & deviance==FALSE){
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
    
    performance <- evaluate_all_measures(object, 
                                         make_expr_and_FUNS(measures, parameters, estimator, alpha, custom_funs, custom_expr_before, custom_expr_after), 
                                         parameters,
                                         progress=progress,
                                         options=options)
    
    no_nas_measures <- c("E", "LQuantile", "Epvar", "Epsd", "var", "se", "cv")
    
  }
  
  performance <- nlist::as_term_frame(performance)
  
  performance$measure = sapply(strsplit(performance$term, "\\."), function(x) x[1])
  performance$term = sapply(strsplit(performance$term, "\\."), function(x) x[-1])
  performance <- data.frame(reshape::cast(performance, term ~ measure))
  if(custom_expr_before=="" & custom_expr_after=="" & !("all" %in% measures)) performance <- performance[,c("term", measures)]
  
  # remove excess measures
  if(any(measures != "all") & any(measures != "")) performance <-  performance[,c("term", measures)]
  # Order alphabetically. This method is consistent across all OS
  # Sort all except term which should be the 1st column
  performance <- performance[, c("term", stringr::str_sort(colnames(performance)[colnames(performance)!="term"]))]
  
  if(!read.file){
        return(performance)
  }else{
    dir <- file.path(path, folder, "performance"); dir.create(dir)
    saveRDS(performance, file.path(dir, "performance.rds"))
  } 
  
}


