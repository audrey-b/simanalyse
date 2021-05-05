

#### 1. Nlists without files
#### 2. Nlists with files

#### Nlists object: sims
#### Files object: path



#############1. Nlists without files

#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. Allows results to be written to files.
#' 
#' @param sims An nlists or nlist object of the data (or list that can be coerced to nlist or nlists).
#' @param code A string of code to analyze the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.
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
#'  result <- sma_analyse(sims=sims,
#'                                        code = code,
#'                                        code.add = prior,
#'                                        mode=sma_set_mode("quick"),
#'                                        monitor = "mu")

sma_analyse <- function(sims,
                               code,
                               code.add="",
                               code.values=NULL,
                               monitor = ".*",
                               inits=list(),
                               mode=sma_set_mode("report"),
                               deviance = TRUE,
                               #pD = FALSE,
                               #save= NA,
                              
                               progress = FALSE,
                               options = furrr::furrr_options(seed = TRUE)){
  
  sma_analyse_internal(sims = sims,
              code,
              code.add,
              code.values,
              monitor,
              inits,
              mode,
              deviance,
              path = NULL,
              analysis = NULL,
              progress = FALSE,
              options
              
  )
  
}


########################################################################

#############2. Nlists with files

#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. Allows data to be read from files and results to be written to files.
#' 
#' @param code A string of code to analyze the data. JAGS code must not be in a data or model block.
#' @param code.add A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.
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
#' @param path A string. Sims are read from that path on disk and results are written to disk.
# @param path.save A string specifying the path to the directory to save the results. By default path = NULL the results are not saved but are returned as a list of nlists objects.
#' @param analysis A string for the name of the folder that contains the results.
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
#'  result <- sma_analyse(sims=sims,
#'                                        code = code,
#'                                        code.add = prior,
#'                                        mode=sma_set_mode("quick"),
#'                                        monitor = "mu")

sma_analyse_files <- function(
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
                               analysis = "analysis0000001",
                               progress = FALSE,
                               options = furrr::furrr_options(seed = TRUE)){
  
  sma_analyse_internal(sims = NULL,
              code,
              code.add,
              code.values,
              monitor,
              inits,
              mode,
              deviance,
              path = path,
              analysis,
              progress = FALSE,
              options
              
  )
  
}


########################################################################


