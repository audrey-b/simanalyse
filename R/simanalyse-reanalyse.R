#' Reanalyse some datasets (might be done through analyse?)
#'
#' Reanalyse some datasets in the case of convergence failure or unexpected errors.
#' 
#' @param path A string specifying the path to the directory where the analyses were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#' @param exists A flag specifying whether the analyses should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#' @param silent A flag specifying whether to suppress warnings.

#' @return A flag.
#' @export
#'
#' @examples
#' sma_reanalyse()
#' sma_reanalyse(FALSE)
# sma_reanalyse <- function(x = TRUE) {
#   check_flag(x)
#   x
# }
# 
