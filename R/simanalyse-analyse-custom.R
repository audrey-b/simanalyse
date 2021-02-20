#' #' Analyse data for a simulation study
#' #'
#' #' Analyse data for a simulation study. If path is supplied, saves a hidden file with the information on the analysis.
#' #'
#' #' @param sims A list of nlist objects containing the data. Alternatively, path can be used to specify to read the data from files.
#' #' @param FUN An R function to analyse the data. It should contain an argument called data which takes an nlist of the data.
#' #' @param path A string specifying the path to the directory where the data sets are saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#' #' @param seed An integer specifying the random seed to use for analysing the data.
#' #' @param exists A flag specifying whether the directory should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#' #' @param silent A flag specifying whether to suppress warnings.
#' #' @param ... Arguments to be passed to \code{FUN}.
#' #'
#' #' @return A flag.
#' #' @export
#' #'
#' #' @examples
#' #' sims <- sims::sims_simulate("a ~ dnorm(0,1)")
#' #' sma_analyse_custom(sims, FUN=function(sims, arg) sims[arg], arg="a")
#'
#' sma_analyse_custom <- function(sims,
#'                                       FUN,
#'                                       path,
#'                                       seed,
#'                                       exists,
#'                                       silent,
#'                                       ...) {
#'   chk_nlists(sims)
#'   lapply(sims, chk_nlist)
#'
#'   if(!missing(sims)) lapply(sims,
#'                                 function(sims, ...) FUN(sims=sims, ...),
#'                                 ...)
#'
#' }
