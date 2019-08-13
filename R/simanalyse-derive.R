#' Apply R code to each element of an nlist object. This can be used to derive new variables or create some missing data in the simulated data 
#' or in the results of the analyses.
#'
#' Derive new variables from the results of analyses
#' 
#' @param code A string of R code to modify the results.
#' @param monitor  A lcharacter vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the data. By default all stochastic nodes are included.
#' @param parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#' @param path A string specifying the path to the directory to save the data sets in. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#' @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'
#' @examples
#' sma_derive()
#' sma_derive(FALSE)
# sma_derive <- function(x = TRUE) {
#   check_flag(x)
#   x
# }

# sma_derive(code = "b <- a + 1", path=...)

