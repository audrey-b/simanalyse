#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. Allows data to be read from files and results to be written to files.
#' 
#' @param sims An nlists or nlist object of the data. If NULL, data files are read from \code{path}.
#' @param code A string of code to analyse the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.
#' @param code.values A character vector to pass to sprintf before analysing the data. This is useful for varying choices of distributions, e.g. for assessing sensitivity to the choice of priors.
#' @param package A string with the name of the R package to analyse the data. Currently, only "rjags" is implemented.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param n.chains An integer greater than zero specifying the number of MCMC chains to run
#' @param inits A list or a function. Initial values for the MCMC chains. If specifying a function, it should either have no arguments, or have a single argument named chain. In the latter case, the supplied function is called with the chain number as argument. In this way, initial values may be generated that depend systematically on the chain number.
#' @param n.adapt An integer specifying the number of adaptations for each analysis
#' @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
#' @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
#' @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
#' @param deviance A flag. Indicates whether to monitor deviance for future DIC calculation.
# @param pD A flag. Indicates whether to monitor pD for future DIC calculation.
#' @param path A string. If \code{sims} is NULL, sims are read from that path on disk and results are written to disk.
# @param path.save A string specifying the path to the directory to save the results. By default path = NULL the results are not saved but are returned as a list of nlists objects.
#' @param analysis If \code{path} is specified, a string for the name of the folder that contains the results.
#' @param progress A flag specifying whether to print a progress bar.
#' @param options The future specific options to use with the workers.
#' 
#' @return A flag.
#' @export
#'
#' @examples
#'  set.seed(10L)
#'  code <- "a ~ dnorm(mu,1)"
#'  sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#'  prior = "mu ~ dunif(-3,3)"
#'  result <- sma_analyse_bayesian(sims=sims,
#'                                        code = code,
#'                                        code.add = prior,
#'                                        n.adapt = 101,
#'                                        n.burnin = 0,
#'                                        n.iter = 101,
#'                                        monitor = "mu")

sma_analyse_bayesian <- function(sims = NULL,
                                 code,
                                 code.add="",
                                 code.values="",
                                 package="rjags",
                                 monitor, #need to configure so .* by default
                                 inits=list(),
                                 n.adapt,
                                 n.burnin,
                                 n.iter,
                                 thin=1,
                                 n.chains=3,
                                 deviance = TRUE,
                                 #pD = FALSE,
                                 path = getOption("sims.path"),
                                 analysis = "analysis0000001",
                                 progress = FALSE,
                                 options = furrr::future_options(seed = TRUE)){
  
  if(!is.null(sims)){
    if(is.nlist(sims)) sims <- nlists(sims)
    chk_is(sims, class="nlists")
    lapply(sims, chk_is, class="nlist")
    n.sims <- length(sims)
  }else{
      chk_string(path)
      n.sims <- length(sims_data_files(path))
    }
  
  
  chk_string(code)
  chk_string(code.add)
  chk_string(code.values)
  chk_string(package)


  chk_is(monitor, "character")
  
  chk_is(inits, class=c("list", "function"))
  #lapply(chk_) need to figure out
  
  chk_whole_number(n.chains); chk_range(n.chains, c(1, .max_integer))
  chk_whole_number(n.adapt); chk_range(n.adapt, c(0, .max_integer))
  chk_whole_number(n.burnin); chk_range(n.burnin, c(0, .max_integer))
  chk_whole_number(n.iter); chk_range(n.iter, c(1, .max_integer))
  
  chk_number(thin); chk_range(thin, c(1, n.iter))
  
  chk_flag(progress)
  chk_s3_class(options, "future_options")
  
  # n.seeds = n.chains*n.sims
  # seeds.unlist <- furrr::future_map(1:n.seeds, function(x) print(.Random.seed), .options = options)
  # seeds = rep(list(list()), n.sims) 
  # for(i in 1:n.sims){
  #     seeds[[i]] = seeds.unlist[[(i-1)*3 + 1]]
  # }
  # options$seed = seeds
  # names(seeds) = chk::p0("sims", 1:5)
  # if(!is.null(path)) saveRDS(seeds, file.path(path, analysis, ".seeds.rds"))
  
  seeds = rinteger(n.sims)
  
  res.list <- list(nlists(nlist()))
  
  code %<>% prepare_code(code.add, code.values)
  
  if(deviance == TRUE){
    load.module("dic")
    monitor <- unique(c(monitor, "deviance"))
  }
  #if(pD == TRUE) monitor <- unique(c(monitor, "pD"))
  
  #jags
  if(is.null(path)){
    #if(!is.null(path) & is.null(sims)) sims <- sims_data(path)
    
    res.list <- future_pmap(list(nlistdata=sims, seed=seeds), analyse_dataset_bayesian, 
                           code=code, monitor=monitor,
                           inits=inits, n.chains=n.chains,
                           n.adapt=n.adapt, n.burnin=n.burnin, 
                           n.iter=n.iter, thin=thin, .progress = progress, .options=options)
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    if(deviance == TRUE) unload.module("dic")
    return((mcmcr::as.mcmcrs(res.list)))
    
  }else{sma_batchr(sma.fun=analyse_dataset_bayesian, seeds=seeds, 
                   path.read=path,
                   path.save=file.path(path, analysis, "results"),
                   prefix="data", suffix="results",
                   code=code, monitor=monitor,
                   inits=inits, n.chains=n.chains,
                   n.adapt=n.adapt, n.burnin=n.burnin, 
                   n.iter=n.iter, thin=thin)  
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    if(deviance == TRUE) unload.module("dic")
  }
}

