context("internal")

# test_that("function_template returns TRUE by default", {
#   expect_true(function_template())
# })
# 
# test_that("function_template returns FALSE if x = FALSE", {
#   expect_false(function_template(x = FALSE))
# })
# 
# test_that("function_template errors x is not a FLAG", {
#   expect_error(function_template(1), "^`x` must be a flag [(]TRUE or FALSE[)][.]$")
# })
# 
# 

test_that("make_code works",{
  expect_identical(prepare_code("a ~ dnorm(%s,1)", "b ~ dnorm(%s,1)", -1, 1),
                   "model{\n\na ~ dnorm(-1,1) \n b ~ dnorm(1,1)\n\n}")  
})

test_that("add_model_block works",{
  expect_identical(add_model_block("a ~ dnorm(0,1)"), "model{\n\na ~ dnorm(0,1)\n\n}")
})

test_that("analyse_dataset_bayesian works",{
  result <- analyse_dataset_bayesian(code = "a ~ dnorm(0,1)",
                                     n.adapt = 101,
                                     n.burnin = 0,
                                     n.iter = 101,
                                     monitor = "a",
                                     seed = 10)
  expect_true(class(result)=="nlists")
  expect_equal(result[[1]] %>% as.numeric, -1.636731, tolerance = 0.000001)
})

test_that("analyse_dataset_bayesian with data works",{
  set.seed(10L)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=2)
  result <- analyse_dataset_bayesian(nlistdata = dat[[1]],
                                     code = "a ~ dnorm(0,1)
                                             mu ~ dnorm(-3, 3)",
                                     n.adapt = 101,
                                     n.burnin = 0,
                                     n.iter = 101,
                                     monitor = "mu",
                                     seed = 10)
  expect_true(class(result)=="nlists")
  expect_equal(result[[1]] %>% as.numeric, -3.944967, tolerance = 0.000001)
})

test_that("package works",{
  set.seed(10L)
  params <- nlist(mu=0)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = params, nsims=2)
  result <- simanalyse_analyse_bayesian(datalist=dat,
                                        code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                        n.adapt = 101,
                                        n.burnin = 0,
                                        n.iter = 101,
                                        monitor = "mu")
  expect_true(class(result)=="list")
  expect_equal(result[[1]][1][[1]]$mu, -1.817165, tolerance = 0.000001)
  
  #summarise_within(result, aggregate_FUN=var)
  
  #simanalyse_summarise(result, "Epsd", parameters=params, monitor="mu")
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
  result <- lapply(listnlists, summarise_within, expr, aggregate.FUNS, parameters) %>%
    as.nlists()
  error.mu1 <- c(mean(mu11), mean(mu12)) - parameters$mu
  error.mu2 <- c(mean(mu21), mean(mu22)) - parameters$mu
  error.theta1 <- mean(theta1) - parameters$theta
  error.theta2 <- mean(theta2) - parameters$theta
  expect_identical(result, nlists(nlist(error.mu = error.mu1,
                                        error.theta=error.theta1),
                                  nlist(error.mu = error.mu2, 
                                        error.theta=error.theta2)))
  result2 <- summarise_all_measures(listnlists, "bias", parameters=parameters, estimator=mean)
  expect_identical(result2, nlist(bias.mu = (error.mu1+error.mu2)/2,
                                   bias.theta = (error.theta1+error.theta2)/2))
})


 test_that("package works 2",{
   set.seed(10L)
   parameters <- nlist(theta=c(0,1), df=3)
   dat <- sims_simulate("a ~ dt(theta[1],theta[2], df)", 
                        parameters = parameters, 
                        nsims=2)
   result <- simanalyse_analyse_bayesian(datalist=dat,
                                         code = "a ~ dt(theta[1],theta[2], df)
                                          theta[1] ~ dunif(-3,3)
                                          theta[2] ~ dunif(0,3)
                                          dfnotrnd ~ dnorm(0,20)I(1,)
                                          df <- round(dfnotrnd)",
                                         n.adapt = 100,
                                         n.burnin = 0,
                                         n.iter = 2,
                                         monitor = c("theta", "df"))
   simanalyse_summarise(result, measures="bias", parameters=parameters, monitor=".*")
   simanalyse_summarise(result, measures="mse", parameters=parameters, monitor=".*")
   simanalyse_summarise(result, measures="cp.quantile", parameters=parameters, monitor=".*")
   simanalyse_summarise(result, measures=c("bias","mse"), parameters=parameters, monitor=".*")
   
 })
 
