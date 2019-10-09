#' Calculates measures to evaluate the performance of the model
#'
#' Calculates measures to evaluate the performance of the model
#' The results are compared with the true parameter values by calculating performance measures 
#' such as bias, mean square error and coverage probability.
#' R code can be used to customize the performance measures
#' 
#' @param object A list of nlists object of results or a single nlists of results. If set to NULL, the object is read from \code{path} instead.
#' @param measures A vector of strings indicating which performance measures to calculate. Strings may include "bias", "E" (expectation), 
#' "cp.quantile" (coverage probability of quantile-based CrIs), "cp.length" (coverage probability of quantile-based CrIs),
#' "Epvar" (expected posterior variance), "Epsd" (expected posterior standard deviation), "rb" (relative 
#'  bias), "br" (bias ratio), "var" (variance), "se" (standard error), rmse (root mean square error), "rrmse" (relative root mean square error),
#' "cv" (coefficient of variation), "all" (all the measures)
#' @param parameters An nlist. True values of parameters to be used to calculate the performance measures.
#' @param estimator A function, typically mean or median, for the Bayes estimator to use to calculate the performance measures.
#' @param alpha Scalar representing the alpha level used to construct credible intervals. Default is 0.05.
#' @param monitor A character vector (or regular expression if a string) specifying the names of the stochastic nodes in code to include in the summary. By default all stochastic nodes are included.
#' @param path A string. If \code{object} is NULL, the object is read using this path. If a "derive" folder exists, the object is read from that folder, otherwise it is read from the "results" folder.
#' @param analysis If \code{path} is used, a string for the name of the folder that contains the analysis.
#' @param custom_funs A named list of functions to calculate over the mcmc samples. E.g. list(posteriormedian = median).
#' @param custom_expr_before A string of R code to derive custom measures. This code is used BEFORE averaging over all simulations. E.g. "mse = (posteriormedian - parameters)^2". Functions from \code{custom_funs} may be used as well as the keywords 'parameters' (the true values of the parameters) and 'estimator' (the estimator defined in \code{estimator}).
#' @param custom_expr_after A string of R code to derive additional custom measures. This code is used AFTER averaging over all simulations. E.g. "rmse = sqrt(mse)". Measures calculated from \code{custom_expr_before} may be used as well as the keyword 'parameters' (the true values of the parameters). 
#' @param progress A flag specifying whether to print a progress bar.
#' @param options The future specific options to use with the workers.
#' 
# @param summaries A nlists of previously calculated summaries. Providing these will speed up this function, as they won't be recalculated.
#' @return A flag.
#' @export
#'
# @examples
# params <- nlist(mu=0)
# dat <- sims::sims_simulate("a ~ dnorm(mu, 1)", 
#                            parameters = params, 
#                            nsims=5)
# result <- sma_analyse_bayesian(sims=dat,
#                                       code = "a ~ dnorm(mu, 1)
#                                               mu ~ dunif(-3,3)",
#                                       mode=sma_set_mode("debug"),
#                                       monitor="mu")
# sma_evaluate(result, parameters=params)
# sma_evaluate(result, 
# measures="", 
# parameters = params, 
# custom_funs = list(posteriormedian = median),
# custom_expr_before = "mse = (posteriormedian - parameters)^2",
# custom_expr_after = "rmse = sqrt(mse)")


#  thin
#  parallel An integer specifying the number of CPU cores to use for generating the datasets in parallel. Defaul is 1 (not parallel).
#  path A string specifying the path to the directory where the results were saved. By default \code{path = NULL } the data sets are not saved but are returned as an nlists object.
#  exists A flag specifying whether the summaries should already exist. If \code{exists = NA} it doesn't matter. If the directory already exists it is overwritten if \code{exists = TRUE} or \code{exists = NA} otherwise an error is thrown.
#  silent A flag specifying whether to suppress warnings.

sma_evaluate <- function(object = NULL, 
                         measures=c("bias", "mse", "cp.quantile"), 
                         estimator=mean, 
                         alpha=0.05,
                         parameters = NULL,
                         monitor=".*",
                         path = ".",
                         analysis = "analysis0000001",
                         custom_funs,
                         custom_expr_before="",
                         custom_expr_after="",
                         progress = FALSE,
                         options = furrr::future_options()){
        
        chk_vector(measures); chk_all(measures, "chk_string")
        chk_function(estimator)
        chk_vector(monitor); chk_all(monitor, chk_string)
        chk_number(alpha); chk_range(alpha, c(0,1))
        chk_string(custom_expr_before)
        chk_string(custom_expr_after)
        chk_flag(progress)
        chk_s3_class(options, "future_options")
        
        if(!missing(custom_funs)){
                chk_list(custom_funs)
                lapply(custom_funs, chk_function)
        }else custom_funs=NULL
        
        read.file=is.null(object)
        
        if(read.file){
                chk_dir(path)
                derive.path <- file.path(path, analysis, "derived")
                if(dir.exists(derive.path)){
                        prefix = "deriv"
                        parameters = readRDS(list.files(path=derive.path, pattern=".parameters.rds", all.files=TRUE, full.names=TRUE))
                }else{                        
                        prefix="results"
                        parameters = sims_info(path)$parameters
                }
                files <- list.files(path=file.path(path, analysis), pattern=p0("^", prefix, "\\d{7,7}.rds$"), recursive=TRUE, full.names=TRUE)
                object <- mcmcr::as.mcmcrs(lapply(files, readRDS))
        }
        
        chk_nlist(parameters)
        
        object %<>% lapply(function(x) as.nlists(mcmcr::collapse_chains(x)))
        chk_is(object, "list"); lapply(object, chk_is, class="nlists")
        
        if(monitor != ".*"){object %<>% lapply(subset, select=monitor)
                #parameters %<>% parameters[monitor]
        }
        
        performance <- evaluate_all_measures(object, 
                                             make_expr_and_FUNS(measures, parameters, estimator, alpha, custom_funs, custom_expr_before, custom_expr_after), 
                                             parameters,
                                             progress=progress,
                                             options=options)
        if(!read.file){
                return(performance)
        }else{
                dir <- file.path(path, "performance"); dir.create(dir)
                saveRDS(performance, file.path(dir, "performance.rds"))
        }  
        
}


