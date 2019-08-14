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

# set.seed(10L)
# dat <- sims_simulate("a ~ dnorm(mu,1) \n f ~ dnorm(0,1) \n j ~ dnorm(0,1)", parameters = nlist(mu=0), nsims=2)
# result <- analyse_dataset_bayesian(nlistdata = dat[[1]],
#                                    code = "a ~ dnorm(0,1)
#                                              mu ~ dnorm(-3, 3)",
#                                    n.adapt = 101,
#                                    n.burnin = 0,
#                                    n.iter = 101,
#                                    monitor = "mu",
#                                    seed = 10)
# dat
# code <- "b=a+1
#         g=f+1
#          k=j+1"
# 
# nlists1 <- nlists(nlist(a=1, b=1), nlist(a=-1,b=-1))
# nlists2 <- mcmc_derive(nlists1, "c=a+1")
# nlists1 %>% lapply(mcmc_derive, expr="c=a+1", silent=TRUE)
# nlists1 %>% subset(select="a")
# 
#  sma_derive <- function(nlists, code, values=list(), monitor=".*") {
#     dat %>% lapply(mcmc_derive, expr=code, silent=TRUE)
#     append(dat, test)}

# sma_derive(code = "b <- a + 1", path=...)

