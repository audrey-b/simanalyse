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
#' @param batch An integer specifying the number of iterations in a batch. New batches are ran until at least one of convergence, max.time or max.iter is reached.
#' @param max.save An integer specifying the maximum number of samples to save. Whenever the saved object is to become larger than max.save, the first iterations are dropped so that the number of saved iterations does not exceed \code{max.save}. After each full cycle of max.save iterations dropped, the number of iterations in new batches is doubled while thinning is increased by a factor of 2 to keep the same effective number of iterations per batch.
#' @param max.iter An integer specifying the maximum number of iterations to run. Iterations that were dropped or thinned out count as iterations in this definition. When that number of iterations is reached no further batches are ran.
#' @param max.time An integer specifying the maximum time in \code{units}. When that time is reached no further batches are ran.
#' @param units Character string specifying the units of time for \code{max.time}. See \code{difftime}.
# @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
# @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
# @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
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
#'                                        batch = 101,
#'                                        max.iter=101,
#'                                        monitor = "mu")

sma_analyse_bayesian <- function(sims = NULL,
                                 code,
                                 code.add="",
                                 code.values="",
                                 package="rjags",
                                 monitor, #need to configure so .* by default
                                 inits=list(),
                                 n.adapt,
                                 #n.burnin,
                                 #n.iter,
                                 #thin=1,
                                 batch=5,
                                 max.save=50000,
                                 max.time=0.2,
                                 units="mins",
                                 max.iter=.max_integer,
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
  #chk_whole_number(n.burnin); chk_range(n.burnin, c(0, .max_integer))
  #chk_whole_number(n.iter); chk_range(n.iter, c(1, .max_integer))
  #chk_number(thin); chk_range(thin, c(1, n.iter))
  chk_whole_number(batch)
  chk_whole_number(max.save)
  chk_whole_number(max.iter); chk_gt(batch, 0)
  chk_number(max.time); chk_gt(max.time, 0)
  #units?
  
  chk_flag(progress)
  chk_s3_class(options, "future_options")
  
  seeds <- furrr::future_map(1:n.sims, 
                             function(x) return(.Random.seed), 
                             .options = options)
  options$seed = seeds
  names(seeds) = chk::p0("sims", 1:n.sims)
  if(!is.null(path)){
    if(!dir.exists(file.path(path, analysis))) dir.create(file.path(path, analysis))
    saveRDS(seeds, file.path(path, analysis, ".seeds.rds"))
  }
  
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
    
    res.list <- future_pmap(list(nlistdata=sims), analyse_dataset_bayesian, 
                            code=code, monitor=monitor,
                            inits=inits, n.chains=n.chains,
                            n.adapt=n.adapt, max.time=max.time,
                            max.iter=max.iter, batch=batch, max.save=max.save,
                            units=units, .progress = progress, .options=options)
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    if(deviance == TRUE) unload.module("dic")
    return((mcmcr::as.mcmcrs(res.list)))
    
  }else{sma_batchr(sma.fun=analyse_dataset_bayesian,
                   path.read=path,
                   analysis=analysis,
                   path.save=file.path(path, analysis, "results"),
                   prefix="data", suffix="results",
                   code=code, monitor=monitor,
                   inits=inits, n.chains=n.chains,
                   n.adapt=n.adapt, max.time=max.time,
                   max.iter=max.iter, batch=batch, max.save=max.save,
                   units=units, options=options)
    
    if("lecuyer::RngStream" %in% list.factories(type="rng")[,1]) unload.module("lecuyer")
    if(deviance == TRUE) unload.module("dic")
  }
}

