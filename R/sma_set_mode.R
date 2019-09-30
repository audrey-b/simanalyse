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

#'   \item{sma.n.adapt}{A count of the number of adaptations}

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
            
            sma.n.adapt = NULL,
            
            sma.max.save = NULL,
            
            sma.batch = NULL,
            
            sma.max.iter = NULL,
            
            sma.rhat = NULL,
            
            sma.esr = NULL,
            
            sma.max.time = NULL,
            
            sma.units = NULL)
    
  } else if (mode == "debug") {
    
    options(sma.n.chains = 2L,
            
            sma.n.adapt = 200L,
            
            sma.max.save = 5L,
            
            sma.batch = 5L,
            
            sma.max.iter = 5L,
            
            sma.rhat = 1.05,
            
            sma.esr = 0.1,
            
            sma.max.time = .Machine$double.xmax,
            
            sma.units = NULL)
    
  } else if (mode == "report") {
    
    options(sma.n.chains = 3L,
            
            sma.n.adapt = 3000,
            
            sma.max.save = 50000,
            
            sma.batch = 100,
            
            sma.max.iter = .Machine$integer.max,
            
            sma.rhat = 1.05,
            
            sma.esr = 0.1,
            
            sma.max.time = .Machine$double.xmax,
            
            sma.units = NULL)
    
  } else if (mode == "paper") {
    
    options(sma.n.chains = 3L,
            
            sma.n.adapt = 10000,
            
            sma.max.save = 50000,
            
            sma.batch = 4000,
            
            sma.max.iter = .Machine$integer.max,
            
            sma.rhat = 1.01,
            
            sma.esr = 0.25,
            
            sma.max.time = .Machine$double.xmax,
            
            sma.units = NULL)
    
  } else err("mode '", mode,"' unrecognised (possible values are 'debug', 'reset', 'report' or 'paper')")
  
}
