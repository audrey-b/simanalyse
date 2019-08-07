#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. If path is supplied, saves a hidden file with the information on the analysis.
#' 
#' @param datalist A list of nlist objects containing the data. Alternatively, path can be used to specify to read the data from files.
#' @param FUN An R function to analyse the data. It should contain an argument called data which takes an nlist of the data.
#' @param code A string of JAGS or R code to analyse the data. The code must not be in a data or model block.
#' @param append A string of JAGS or R code to append at the end of \code{code} before analysing the data. This is useful for specifying priors seperately from the likelihood in JAGS.
#' @param sprintf_args A list of arguments to pass to sprintf when applied to the appended code
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.
#' @param inits aa
#' @param n.adapt aa
#' @param n.burnin aa
#' @param n.iter aa
#' @param thin aa
#' @param path A string specifying the path to the directory where the data sets are saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#' @param seed A positive integer specifying the random seed to use for analysing the data.
#' @param exists A flag specifying whether the directory should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#' @param silent A flag specifying whether to suppress warnings.
#' @param ... Arguments to be passed to \code{FUN}.
#' 
#' @return A flag.
#' @export
#'
#' @examples
#' dat <- sims::sims_simulate("a ~ dnorm(0,1)")
#' simanalyse_analyse_bayesian(dat, FUN=function(data, arg) data[arg], arg="a")

simanalyse_analyse_bayesian <- function(datalist,
                                        FUN,
                                        code,
                                        append,
                                        sprintf_args,
                                        monitor,
                                        inits,
                                        n.adapt,
                                        n.burnin,
                                        n.iter,
                                        thin,
                                        seed,
                                        path,
                                        exists,
                                        silent,
                                        ...) {
  
  
  check_nlists(datalist)
  lapply(datalist, check_nlist)
  
  if(!missing(datalist)) NULL
  
}
