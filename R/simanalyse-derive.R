#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param nlists A nlists object
#' @param code A string of R code to modify the results.
#' @param monitor  A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the data. By default all stochastic nodes are included.
# @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
# @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
# @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'
#' @examples
#' sma_derive()
#' sma_derive(FALSE)

# sma_derive <- function(object, code, monitor=".*") {
#   if(monitor == ".*") monitor = names(object[[1]])
#   object %>% 
#     mcmc_derive(expr = code, 
#                 primary = TRUE, 
#                 silent = TRUE) %>%
#     subset(select=monitor) %>%
#     return
# }

