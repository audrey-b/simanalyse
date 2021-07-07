library(testthat)
library(simanalyse)

context("internal")

test_that("make_code works",{
  expect_identical(prepare_code("a ~ dnorm(?,1)", 
                                c("b ~ dnorm(?,sigma)", "sigma=1"), 
                                c(-1,1)),
                   "a ~ dnorm(-1,1) \nb ~ dnorm(1,sigma) \nsigma=1") 
})

test_that("add_model_block works",{
  expect_identical(add_model_block("a ~ dnorm(0,1)"), "model{\n\na ~ dnorm(0,1)\n\n}")
})


test_that("summarise one measure - bias",{
  parameters = nlist(mu=c(0,0), theta=1)
  mu11 = c(-1,1) #two mcmc iterations of mu1 for sim1
  mu12 = c(-2,2) #two mcmc iterations of mu2 for sim1
  mu21 = c(1,2) #two mcmc iterations of mu1 for sim2
  mu22 = c(-2,-1) #two mcmc iterations of mu2 for sim2
  theta1 = c(1,3) #two mcmc iterations of theta for sim1
  theta2 = c(0,2) #two mcmc iterations of theta for sim2
  nlists1 <- nlists(nlist(mu=c(mu11[1],mu12[1]), theta=theta1[1]), nlist(mu=c(mu11[2],mu12[2]), theta=theta1[2]))
  nlists2 <- nlists(nlist(mu=c(mu21[1],mu22[1]), theta=theta2[1]), nlist(mu=c(mu21[2],mu22[2]), theta=theta2[2]))
  aggregate.FUNS=list(estimator = mean)
  listnlists <- list(nlists1,nlists2)
  #bias
  expr="error = estimator - parameters"
  result <- lapply(listnlists, evaluate_within, expr, aggregate.FUNS, parameters) %>%
    as_nlists()
  error.mu1 <- c(mean(mu11), mean(mu12)) - parameters$mu
  error.mu2 <- c(mean(mu21), mean(mu22)) - parameters$mu
  error.theta1 <- mean(theta1) - parameters$theta
  error.theta2 <- mean(theta2) - parameters$theta
  expect_identical(result, nlists(nlist(error.mu = error.mu1,
                                        error.theta=error.theta1),
                                  nlist(error.mu = error.mu2, 
                                        error.theta=error.theta2)))
  
  expr_FUN <- make_expr_and_FUNS("bias", estimator = mean, parameters = parameters)
  expect_identical(expr_FUN[["expr"]], "bias = estimator - parameters")
  result2 <- evaluate_all_measures(listnlists, expr_FUN, parameters = parameters)
  expect_identical(result2, nlist(bias.mu = (error.mu1+error.mu2)/2,
                                  bias.theta = (error.theta1+error.theta2)/2))
})





test_that("analyse_dataset_bayesian with data works",{
  set.seed(10L)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=2)
  result <- analyse_dataset_bayesian(nlistdata = dat[[1]],
                                     code = "a ~ dnorm(0,1)
                                             mu ~ dnorm(-3, 3)",
                                     n.adapt = 101,
                                     max.iter=5,
                                     n.save=5,
                                     max.time=0.1,
                                     monitor = "mu",
                                     deviance = TRUE,
                                     ess=5,
                                     r.hat=1.5,
                                     r.hat.nodes = ".*",
                                     ess.nodes=".*",
                                     normalize = FALSE)
  
  result1 <- readRDS("Internal_results/result1.rds")
  expect_equal(result, result1)
})

test_that("analyse_dataset_bayesian works",{
  result <- analyse_dataset_bayesian(code = "a ~ dnorm(0,1)",
                                     n.adapt = 101,
                                     max.iter=5,
                                     n.save=5,
                                     max.time=0.1,
                                     monitor = "a",
                                     deviance = TRUE,
                                     ess=5,
                                     r.hat=1.5,
                                     r.hat.nodes = ".*",
                                     ess.nodes=".*",
                                     normalize = FALSE)
  
  result2 <- readRDS("Internal_results/result2.rds")
  expect_equal(result, result2)
})

test_that("inits as function",{
  set.seed(10L)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=2)
  result <- analyse_dataset_bayesian(nlistdata = dat[[1]],
                                     code = "a ~ dnorm(0,1)
                                             mu ~ dnorm(-3, 3)",
                                     n.adapt = 101,
                                     max.iter=5,
                                     n.save=5,
                                     max.time=0.1,
                                     monitor = "mu",
                                     deviance = TRUE,
                                     ess=5,
                                     r.hat=1.5,
                                     r.hat.nodes = ".*",
                                     ess.nodes=".*",
                                     normalize = FALSE)
  
  result1 <- readRDS("Internal_results/result1.rds")
  expect_equal(result, result1)
})

test_that("inits as function of chain and data",{
  set.seed(10L)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=1)
  
  inits <- function(){0}
  expect_equal(unname(set_seed_inits(inits, 3, dat[[1]])[1:3]), list(0, 0, 0))
  
  inits <- function(chain){if(chain == 1) return(1); return(0)}
  expect_equal(unname(set_seed_inits(inits, 3, dat[[1]])[1:3]), list(1, 0, 0))
  
  inits <- function(chain, data){mu = data$a}
  expect_equal(unname(set_seed_inits(inits, 3, dat[[1]])[1:3]), list(dat[[1]]$a, dat[[1]]$a, dat[[1]]$a))
})

# test_that("extract_measure_from_summary",{
#   monitor = c("mu", "theta")
#   word <- "mse"
#   summaries=nlist("bias.mu"=1, "bias.theta"=2, "mse.mu"=3, "mse.theta"=4)
#   extract_measure_from_summary(summaries, word, monitor) %>%
#     expect_identical(nlist(mu=3, theta=4))
# })
# 
# test_that("summary_reformat",{
#   measures=c("mse", "bias")
#   monitor = c("mu", "theta")
#   summaries=nlist("bias.mu"=1, "bias.theta"=2, "mse.mu"=3, "mse.theta"=4)
#   res <- summary_reformat(measures=measures,
#                           summaries=summaries,
#                           monitor=monitor) %>%
#     set_names(measures)
#   expect_identical(res, list(mse=nlist(mu=3, theta=4), bias=nlist(mu=1, theta=2)))
# })
