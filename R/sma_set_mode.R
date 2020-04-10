#' Set mode for data analysis
#'
#' Set mode for data analysis. The algorithm starts with an adaptation phase of \code{n.adapt} iterations.
#' Then \code{n.chains} are ran for \code{n.save} iterations without thinning. The algorithm stops if convergence
#' was achieved according to \code{r.hat} and the minimum effective sampling rate was achieved or \code{max.time}
#' was reached or is projected to be reached in the next round of iterations. If the algorithm is not stopped,
#' the iterations so far are discarded as burnin. The number of iterations is doubled from the previous round
#' and thinning is multiplied by 2. The stop criteria is applied again and so on.
#' 
#' @param mode A string of the analysis mode. One of "quick", "report" or "paper". The mode determines the values of the parameters below unless they are specified.
#' @param n.chains A count for number of chains.
#' @param n.adapt A count for the number of adaptations.
#' @param n.save A count for the number of (potentially thinned) samples to save.
#' @param max.iter A count for the maximum number of iterations in total per chain.
#' @param r.hat A number specifying the rhat threshold.
#' @param r.hat.nodes A character vector of nodes to be used to assess r.hat
#' @param ess A number specifying the minimum effective sample size.
#' @param ess.nodes A character vector of nodes to be used to assess ess.
#' @param max.time A number specifying the maximum time to spend on analysis.
#' @param units A character string specifying the units of time for \code{max.time}. See \code{difftime}.

#' The possible modes are as follows:

#' \describe{

#'   \item{'quick'}{To rapidly identify problems with a model definition.}

#'   \item{'report'}{To produce results for a report.}

#'   \item{'paper'}{To produce results for a peer-reviewed paper.}

#' }

#' @export

#' @examples

#' \dontrun{

#' sma_set_mode("quick")

#' }

sma_set_mode <- function(mode = "report",
                         n.chains,
                         n.adapt,
                         n.save,
                         max.iter,
                         r.hat,
                         r.hat.nodes,
                         ess,
                         ess.nodes,
                         max.time,
                         units){
  
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
  if(!missing(n.save)) chk_whole_number(n.save)
  if(!missing(max.iter)) chk_whole_number(max.iter)
  if(!missing(max.time)){
    chk_number(max.time)
    chk_gt(max.time, 0)}
  if(!missing(r.hat.nodes)) chk_character(r.hat.nodes)
  if(!missing(ess.nodes)) chk_character(ess.nodes)
  
  #units?
  #other checks
  
  if (mode == "quick") {
    
    ess = ifelse(missing(ess), 1, ess)
    
    list(n.chains = ifelse(missing(n.chains), 2L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 200L, n.adapt),
         
         ess = ess,
         
         ess.nodes  = ifelse(missing(ess.nodes), ".*", ess.nodes),

         r.hat = ifelse(missing(r.hat), 1.05, r.hat),
         
         r.hat.nodes  = ifelse(missing(r.hat.nodes), ".*", r.hat.nodes),
         
         n.save = ifelse(missing(n.save), 10*ess, n.save),
         
         max.iter = ifelse(missing(max.iter), 10*ess, max.iter),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
  } else if (mode == "report") {
    
    ess = ifelse(missing(ess), 400, ess)
    
    list(n.chains = ifelse(missing(n.chains), 3L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 5000L, n.adapt),
         
         ess = ess,
         
         ess.nodes  = ifelse(missing(ess.nodes), ".*", ess.nodes),
         
         r.hat = ifelse(missing(r.hat), 1.05, r.hat),
         
         r.hat.nodes  = ifelse(missing(r.hat.nodes), ".*", r.hat.nodes),
         
         n.save = ifelse(missing(n.save), 10*ess, n.save),
         
         max.iter = ifelse(missing(max.iter), .Machine$integer.max, max.iter),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
    
  } else if (mode == "paper") {
    
    ess = ifelse(missing(ess), 1000, ess)
    
    list(n.chains = ifelse(missing(n.chains), 3L, n.chains),
         
         n.adapt = ifelse(missing(n.adapt), 10000L, n.adapt),
         
         ess = ess,
         
         ess.nodes  = ifelse(missing(ess.nodes), ".*", ess.nodes),
         
         r.hat = ifelse(missing(r.hat), 1.01, r.hat),
         
         r.hat.nodes  = ifelse(missing(r.hat.nodes), ".*", r.hat.nodes),
         
         n.save = ifelse(missing(n.save), 10*ess, n.save),
         
         max.iter = ifelse(missing(max.iter), .Machine$integer.max, max.iter),
         
         max.time = ifelse(missing(max.time), .Machine$double.xmax, max.time),
         
         units = ifelse(missing(units), "mins", units))
    
  } else err("mode '", mode,"' unrecognised (possible values are 'quick', 'report' or 'paper')")
  
}
