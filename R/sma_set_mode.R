#' Set mode for data analysis
#'
#' Set mode for data analysis
#' 
#' @param mode A string of the analysis mode. One of "debug", "report" or "paper". The mode determines the values of the parameters below unless they are specified.
#' @param n.chains A count for number of chains.
#' @param n.adapt A count for the number of adaptations.
#' @param max.save A count for the number of maximum samples to save.
#' @param max.iter A count for the maximum number of iterations in total per chain.
#' @param r.hat A number specifying the rhat threshold.
#' @param esr A number specifying the minimum effective sampling rate.
#' @param max.time A number specifying the maximum time to spend on analysis.
#' @param units A character string specifying the units of time for \code{max.time}. See \code{difftime}.
#' @param batch A count specifying the number of iterations per batch.

#' The possible modes are as follows:

#' \describe{

#'   \item{'debug'}{To rapidly identify problems with a model definition.}

#'   \item{'report'}{To produce results for a report.}

#'   \item{'paper'}{To produce results for a peer-reviewed paper.}

#' }

#' @export

#' @examples

#' \dontrun{

#' sma_set_mode("debug")

#' }

sma_set_mode <- function(mode = "report",
                         n.chains,
                         n.adapt,
                         max.save,
                         max.iter,
                         r.hat,
                         esr,
                         max.time,
                         units,
                         batch){
  
  chk_string(mode)
  if(!missing(n.chains)){
    chk_whole_number(n.chains)
    chk_range(n.chains, c(1, .max_integer))}
  if(!missing(n.adapt)){
    chk_whole_number(n.adapt)
    chk_range(n.adapt, c(0, .max_integer))}
  #chk_whole_number(n.burnin); chk_range(n.burnin, c(0, .max_integer))
  #chk_whole_number(n.iter); chk_range(n.iter, c(1, .max_integer))
  #chk_number(thin); chk_range(thin, c(1, n.iter))
  if(!missing(batch)) chk_whole_number(batch)
  if(!missing(max.save)) chk_whole_number(max.save)
  if(!missing(max.iter)) chk_whole_number(max.iter)#; chk_gt(batch, 0)
  if(!missing(max.time)){
    chk_number(max.time)
    chk_gt(max.time, 0)}
  #units?
  #other checks
  
  if (mode == "debug") {
    
    list(n.chains = ifelse(missing(n.chains), 2L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 200L, n.adapt),
         
         max.save = ifelse(missing(max.save), 10L, max.save),
         
         batch = ifelse(missing(batch), 10L, batch),
         
         max.iter = ifelse(missing(max.iter), 10L, max.iter),
         
         r.hat = ifelse(missing(r.hat), 1.05, r.hat),
         
         esr = ifelse(missing(esr), 0.1, esr),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
  } else if (mode == "report") {
    
    list(n.chains = ifelse(missing(n.chains), 3L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 3000L, n.adapt),
         
         max.save = ifelse(missing(max.save), 50000L, max.save),
         
         batch = ifelse(missing(batch), 2000L, batch),
         
         max.iter = ifelse(missing(max.iter), .Machine$integer.max, max.iter),
         
         r.hat = ifelse(missing(r.hat), 1.05, r.hat),
         
         esr = ifelse(missing(esr), 0.1, esr),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
    
  } else if (mode == "paper") {
    
    list(n.chains = ifelse(missing(n.chains), 3L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 10000L, n.adapt),
         
         max.save = ifelse(missing(max.save), 50000L, max.save),
         
         batch = ifelse(missing(batch), 10000L, batch),
         
         max.iter = ifelse(missing(max.iter), .Machine$integer.max, max.iter),
         
         r.hat = ifelse(missing(r.hat), 1.01, r.hat),
         
         esr = ifelse(missing(esr), 0.25, esr),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
  } else err("mode '", mode,"' unrecognised (possible values are 'debug', 'report' or 'paper')")
  
}
