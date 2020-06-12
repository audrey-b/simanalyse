
test_that("sma_evaluate",{
  set.seed(10L)
  parameters <- nlist(theta=c(0,1), df=3)
  dat <- sims_simulate("a ~ dt(theta[1],theta[2], df)",
                       parameters = parameters,
                       nsims=2)
  result <- sma_analyse(sims=dat,
                                 code = "a ~ dt(theta[1],theta[2], df)
                                          theta[1] ~ dunif(-3,3)
                                          theta[2] ~ dunif(0,3)
                                          dfnotrnd ~ dnorm(0,20)I(1,)
                                          df <- round(dfnotrnd)",
                                 mode=sma_set_mode("quick"),
                                 monitor = c("theta", "df"),
                                 deviance=FALSE)
  sma_evaluate(result, measures="bias", parameters=parameters, monitor=".*")
  sma_evaluate(result, measures="mse", parameters=parameters, monitor=".*")
  sma_evaluate(result, measures="cpQuantile", parameters=parameters, monitor=".*")
  sma_evaluate(result, measures=c("bias","mse"), parameters=parameters, monitor=".*")
})



test_that("custom expr and FUNS",{
  set.seed(10L)
  parameters <- nlist(mu=0)
  dat <- sims::sims_simulate("a ~ dnorm(mu, 1)",
                             parameters = parameters,
                             nsims=2)
  result <- sma_analyse(sims=dat,
                                 code = "a ~ dnorm(mu, 1)
                                         mu ~ dunif(-3,3)",
                                 mode=sma_set_mode("quick"),
                                 monitor="mu",
                                 deviance=FALSE)
  result_method1 <- sma_evaluate(result, measures="",
                               parameters=parameters,
                               custom_funs= list(estimator = mean),
                               custom_expr_before= "mse = (estimator - parameters)^2",
                               custom_expr_after= "rmse = sqrt(mse)")
  result_method2 <- sma_evaluate(result, measures=c("mse","rmse"), parameters=parameters)
  
  expect_identical(result_method1, result_method2)
  
})