#' Analyse data for a simulation study
#'
#' Analyse data for a simulation study. If path is supplied, saves a hidden file with the information on the analysis.
#' 
#' @param datalist A list of nlist objects containing the data. Alternatively, path can be used to specify to read the data from files.
#' @param package A string with the name of the R package to analyse the data, among the following ones: "rjags"
#' @param code A string of code to analyse the data. JAGS code must not be in a data or model block.
#' @param code_add A string of code to append at the end of \code{code} before analysing the data. This is useful for specifying priors seperately from the likelihood in JAGS.
#' @param code_values A character vector to pass to sprintf when applied to the appended code from \code{code} and \code{code_add}
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
#' @param ... not sure if will use
#' 
#' @return A flag.
#' @export
#'
#' @examples
#' dat <- sims::sims_simulate("a ~ dnorm(0,1)")
#' simanalyse_analyse_bayesian(dat, FUN=function(data, arg) data[arg], arg="a")

simanalyse_analyse_bayesian <- function(datalist,
                                        code,
                                        code_add=NULL,
                                        code_values="",
                                        package,
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
