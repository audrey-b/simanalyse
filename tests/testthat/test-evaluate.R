library(testthat)
library(simanalyse)

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

test_that("evaluate-deviance",{
  constants  <- list(T = 4,                       #number of release occasions
                     R = c(192, 154, 131, 184))   #number of releases
  
  params_M0 <- list(p   = rep(0.5, 4),            #capture probability
                    phi = c(0.8, 0.7, 0.9, 0.8))  #survival probability
  code <- "
for(i in 1:T){
 #multinomial likelihood
 m[i,(i+1):(T+2)] ~ dmulti(q[i,(i+1):(T+2)], R[i])
 #multinomial probabilities
 q[i,(i+1)] = phi[i]*p[i]                              #diagonal
 for(j in 1:i){q[i,j] = 0}                             #lower triangle
 q[i,(T+2)] = 1-sum(q[i,1:(T+1)])}                     #never recaptured
for(i in 1:(T-1)){                                     #upper triangle
 for(j in (i+2):(T+1)){q[i,j] = prod(phi[i:(j-1)])*prod(1-p[i:(j-2)])*p[j-1]}}" 
  
  priors_M0 <- "for(j in 1:T){
                p[j] = p0              #capture probability is constant
                phi[j] ~ dunif(0,1)}   #prior on survival probability
                
              p0 ~ dunif(0,1)" #prior on capture probability
  
  monitor_M0 <- c("phi", "p0")
  
  set.seed(20200624)
  
  sims <- sims::sims_simulate(code=code,
                      constants=constants,
                      parameters=params_M0,
                      monitor="m",
                      nsims = 2,                  #number of datasets per setup
                      exists = NA,                  #allow to overwrite directory
                      ask = FALSE)      #do not ask if ok to overwrite directory 
  
  analyse <- sma_analyse(sims, 
              code, 
              code.add=priors_M0,
              mode = simanalyse::sma_set_mode("quick"),  # the mcmc mode
              monitor=c("phi", "p0")) 
  
  params_M0 <- list(p0   = 0.5,            #capture probability
                    phi = c(0.8, 0.7, 0.9, 0.8))  #survival probability
  
  ev <- sma_evaluate(analyse,
               parameters= params_M0,
               measures = c("all"), 
               alpha = 0.05)
  expect_equal(ev$mse, ev$bias^2 + ev$var)
  
})
