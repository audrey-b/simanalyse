#' Set mode for data analysis
#'
#' Set mode for data analysis
#' 

#' The possible modes are as follows:

#' \describe{

#'   \item{'debug'}{To rapidly identify problems with a model definition.}

#'   \item{'report'}{To produce results for a report.}

#'   \item{'paper'}{To produce results for a peer-reviewed paper.}

#'   \item{'reset'}{To reset all the options to NULL so that they are the default values for each function call.}

#' }

#'

#' In each case the mode is a unique combination of the following package options

#' \describe{

#'   \item{sma.n.chains}{A count of the number of chains.}

#'   \item{sma.max.save}{A count of the number of simulations to save per chain.}

#'   \item{sma.max.iter}{A count specifying the maximum number of iterations.}

#'   \item{sma.rhat}{A number specifying the rhat threshold.}

#'   \item{sma.esr}{A number specifying the minimum effective sampling rate.}

#'   \item{sma.max.time}{The maximum total time to spend on analysis and reanalysis.}

#' }

#'

#' @param mode A string of the analysis mode.

#' @export

#' @examples

#' \dontrun{

#' sma_set_mode("debug")

#' }

sma_set_mode <- function(mode = "reset"){
  
  chk_string(mode)
  
  if (mode == "reset") {
    
    options(sma.n.chains = NULL,
            
            sma.max.save = NULL,
            
            sma.batch = NULL,
            
            sma.max.iter = NULL,
            
            sma.rhat = NULL,
            
            sma.esr = NULL,
            
            sma.max.time = NULL)
    
  } else if (mode == "debug") {
    
    options(sma.n.chains = 2L,
            
            sma.max.save = 10L,
            
            sma.batch = 10L,
            
            sma.max.iter = 10L,
            
            sma.rhat = 1.05,
            
            sma.esr = 0.1,
            
            sma.max.time = NULL)
    
  } else if (mode == "report") {
    
    options(sma.n.chains = 3L,
            
            sma.max.save = 50000,
            
            sma.batch = 100,
            
            sma.max.iter = NULL,
            
            sma.rhat = 1.05,
            
            sma.esr = 0.1,
            
            sma.max.time = NULL)
    
  } else if (mode == "paper") {
    
    options(sma.n.chains = 3L,
            
            sma.max.save = NULL,
            
            sma.batch = NULL,
            
            sma.max.iter = NULL,
            
            sma.rhat = 1.01,
            
            sma.esr = 0.25,
            
            sma.max.time = NULL)
    
  } else err("mode '", mode,"' unrecognised (possible values are 'debug', 'reset', 'report' or 'paper')")
  
}
