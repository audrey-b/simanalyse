#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. Allows data to be read from files and results to be written to files.
#'
#' @param sims An nlists or nlist object of the data (or list that can be coerced to nlist or nlists). If NULL, data files are read from `path`.
#' @param code A string of code to analyze the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of `code` before analysing the data. This is useful for adding priors to the likelihood.
#' @param code.values A character vector to replace all instances of "?" in the model. This is useful for varying choices of distributions, e.g. for assessing sensitivity to the choice of priors.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param inits A list or a function. Initial values for the MCMC chains. If specifying a function, it should either have no arguments, or have a single argument named chain. In the latter case, the supplied function is called with the chain number as argument. In this way, initial values may be generated that depend systematically on the chain number.
#' @param mode A list obtained from sma_set_mode which sets the parameters of the mcmc sampling.
# @param n.burnin An integer specifying the number of burn-in iterations for each analysis (following the adaptation phase)
# @param n.iter An integer specifying the number of iterations for each analysis (following the burn-in phase)
# @param thin A numeric scalar of at least 1 specifying the thinning factor. Default is 1.
#' @param deviance A flag. Indicates whether to monitor deviance for future DIC calculation.
# @param pD A flag. Indicates whether to monitor pD for future DIC calculation.
# @param save A flag specifying whether to save the results in \code{path}. If save = NA the results are saved in \code{path} only if \code{sims} is NULL.
#' @param path A string. If `sims` is NULL, sims are read from that path on disk and results are written to disk.
# @param path.save A string specifying the path to the directory to save the results. By default path = NULL the results are not saved but are returned as a list of nlists objects.
#' @param analysis If `path` is specified, a string for the name of the folder that contains the results.
#' @param progress A flag specifying whether to print a progress bar.
#' @param options The future specific options to use with the workers.
#'
#' @return A flag.
#' @export
#'
#' @examples
#' set.seed(10L)
#' code <- "a ~ dnorm(mu,1)"
#' sims <- sims::sims_simulate(code, parameters = nlist(mu = 0), nsims = 2)
#' prior <- "mu ~ dunif(-3,3)"
#' result <- sma_analyse(
#'   sims = sims,
#'   code = code,
#'   code.add = prior,
#'   mode = sma_set_mode("quick"),
#'   monitor = "mu"
#' )
sma_analyse <- function(sims = NULL,
                        code,
                        code.add = "",
                        code.values = NULL,
                        monitor = ".*",
                        inits = list(),
                        mode = sma_set_mode("report"),
                        deviance = TRUE,
                        # pD = FALSE,
                        # save= NA,
                        path = ".",
                        analysis = "analysis0000001",
                        progress = FALSE,
                        options = furrr::furrr_options(seed = TRUE)) {
  if (!is.null(sims)) {
    if (is.list(sims) && !is_nlist(sims) && !is_nlists(sims) && length(lengths(sims)) == 1) {
      class(sims) <- "nlist"
      chk_nlist(sims)
      sims <- nlists(sims)
    }
    if (is.list(sims) && !is_nlist(sims) && !is_nlists(sims) && length(lengths(sims)) > 1) {
      class(sims) <- "nlists"
      for (i in 1:length(sims)) class(sims[[i]]) <- "nlist"
    }
    chk_nlists(sims)
    n.sims <- length(sims)
  } else {
    chk_string(path)
    n.sims <- length(sims_data_files(path))
  }


  chk_string(code)
  chk_string(code.add)
  if (!is.null(code.values)) chk_all(code.values, chk_string)
  chk_character(monitor)
  chkor(chk_list(inits), chk_function(inits))
  # lapply(chk_) need to figure out
  chk_list(mode)
  # chk_lgl(save)

  # need to check that r.hat.node and ess.nodes are contained within monitor

  chk_flag(progress)
  chk_s3_class(options, "furrr_options")


  if (!is.list(options$seed)) { # error if list not the right length
    seeds <- furrr::future_map(1:n.sims,
      function(x) {
        return(.Random.seed)
      },
      .options = options
    )
    names(seeds) <- chk::p0("data", sprintf("%07d", 1:n.sims), ".rds")
    options$seed <- seeds
  }

  if (is.null(sims)) {
    if (!dir.exists(file.path(path, analysis))) dir.create(file.path(path, analysis))
    saveRDS(seeds, file.path(path, analysis, ".seeds.rds"))
  }

  res.list <- list(nlists(nlist()))

  code %<>% prepare_code(code.add, code.values)

  if (deviance == TRUE) {
    load.module("dic")
    monitor <- unique(c(monitor, "deviance"))
  }
  # if(pD == TRUE) monitor <- unique(c(monitor, "pD"))

  # jags
  if (!is.null(sims)) {
    # if(!is.null(path) & is.null(sims)) sims <- sims_data(path)

    res.list <- future_pmap(list(nlistdata = sims), analyse_dataset_bayesian,
      code = code, monitor = monitor,
      inits = inits, n.chains = mode$n.chains,
      n.adapt = mode$n.adapt, max.time = mode$max.time,
      max.iter = mode$max.iter, n.save = mode$n.save,
      ess = mode$ess, r.hat = mode$r.hat,
      ess.nodes = mode$ess.nodes,
      r.hat.nodes = mode$r.hat.nodes,
      units = mode$units, .progress = progress, .options = options
    )

    if ("lecuyer::RngStream" %in% list.factories(type = "rng")[, 1]) unload.module("lecuyer")
    if (deviance == TRUE) unload.module("dic")
    return((mcmcr::as.mcmcrs(res.list)))
  } else {
    options$seed <- FALSE
    sma_batchr(
      sma.fun = analyse_dataset_bayesian,
      path.read = path,
      analysis = analysis,
      path.save = file.path(path, analysis, "results"),
      prefix = "data", suffix = "results",
      code = code, monitor = monitor,
      inits = inits, n.chains = mode$n.chains,
      n.adapt = mode$n.adapt, max.time = mode$max.time,
      max.iter = mode$max.iter, n.save = mode$n.save,
      ess = mode$ess, r.hat = mode$r.hat,
      ess.nodes = mode$ess.nodes,
      r.hat.nodes = mode$r.hat.nodes,
      units = mode$units, options = options, seeds = seeds
    )

    if ("lecuyer::RngStream" %in% list.factories(type = "rng")[, 1]) unload.module("lecuyer")
    if (deviance == TRUE) unload.module("dic")
  }

  future::resetWorkers(future::plan())
}
