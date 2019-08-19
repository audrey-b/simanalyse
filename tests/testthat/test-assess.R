
test_that("sma_assess",{
  set.seed(10L)
  parameters <- nlist(theta=c(0,1), df=3)
  dat <- sims_simulate("a ~ dt(theta[1],theta[2], df)",
                       parameters = parameters,
                       nsims=2)
  result <- sma_analyse_bayesian(sims=dat,
                                 code = "a ~ dt(theta[1],theta[2], df)
                                          theta[1] ~ dunif(-3,3)
                                          theta[2] ~ dunif(0,3)
                                          dfnotrnd ~ dnorm(0,20)I(1,)
                                          df <- round(dfnotrnd)",
                                 n.adapt = 100,
                                 n.burnin = 0,
                                 n.iter = 2,
                                 monitor = c("theta", "df"))
  sma_assess(result, measures="bias", parameters=parameters, monitor=".*")
  sma_assess(result, measures="mse", parameters=parameters, monitor=".*")
  sma_assess(result, measures="cp.quantile", parameters=parameters, monitor=".*")
  sma_assess(result, measures=c("bias","mse"), parameters=parameters, monitor=".*")
})



test_that("custom expr and FUNS",{
  set.seed(10L)
  parameters <- nlist(mu=0)
  dat <- sims::sims_simulate("a ~ dnorm(mu, 1)",
                             parameters = parameters,
                             nsims=2)
  result <- sma_analyse_bayesian(sims=dat,
                                 code = "a ~ dnorm(mu, 1)
                                         mu ~ dunif(-3,3)",
                                 n.adapt = 101,
                                 n.burnin = 0,
                                 n.iter = 101,
                                 monitor="mu")
  result_method1 <- sma_assess(result, measures="",
                               parameters=parameters,
                               custom_funs= list(estimator = mean),
                               custom_expr_before= "mse = (estimator - parameters)^2",
                               custom_expr_after= "rmse = sqrt(mse)")
  result_method2 <- sma_assess(result, measures=c("mse","rmse"), parameters=parameters)
  
  expect_identical(result_method1, result_method2)
  
})