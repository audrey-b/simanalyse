library(testthat)
library(simanalyse)

context("analyse-bayesian")

test_that("sma_analyse",{
   set.seed(10L)
   params <- nlist(mu=0)
   dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = params, nsims=2)
   result <- sma_analyse(sims=dat,
                                  code = "a ~ dnorm(mu,?)
                                         mu ~ dunif(-3,?)",
                                  code.value = c("1","3"),
                                  mode=sma_set_mode("quick"),
                                  monitor = "mu")
   
   result1 <- readRDS("Bayesian_results/result1.rds")
   
   expect_equal(result, result1)
})




test_that("inits is a list of inits for multiple chains",{
   set.seed(10L)
   code <- "a ~ dnorm(mu,1)"
   sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
   prior = "mu ~ dunif(-3,3)
           tt ~ dnorm(0,1)"
   inits1 <- list("mu"=-2, "tt"=0)
   inits2 <- list("mu"=-2, "tt"=0)
   inits3 <- list("mu"=-2, "tt"=0)
   result <- sma_analyse(sims=sims,
                                  code = code,
                                  code.add = prior,
                                  mode=sma_set_mode("quick", n.chains=3),
                                  monitor = c("mu", "tt"),
                                  inits=list(inits1, inits2, inits3),
                                  deviance=FALSE)
   
   result2 <- readRDS("Bayesian_results/result2.rds")
   
   expect_equal(result, result2)
})


test_that("inits is a function and reproducible",{
   set.seed(10L)
   code <- "a ~ dnorm(mu,1)"
   sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
   prior = "mu ~ dunif(-3,3)
           tt ~ dnorm(0,1)"
   
   inits.fun <- function(){
      mu = runif(1, -3, 3)
      tt = rnorm(1, 0, 1)
      return(list("mu"=mu, "tt"=tt))
   }
   
   set.seed(99L)
   result <- sma_analyse(sims=sims,
                                   code = code,
                                   code.add = prior,
                                   mode=sma_set_mode("quick"),
                                   monitor = c("mu", "tt"),
                                   inits=inits.fun,
                                   deviance=FALSE)
   
   result3 <- readRDS("Bayesian_results/result3.rds")
   
   expect_equal(result, result3)

})

test_that("modulo_in_code and default monitor",{
   set.seed(10L)
   params <- nlist(mu=0)
   dat <- sims_simulate("a ~ dnorm(mu,10) \n b = 10 %*% 2", parameters = params, nsims=2)
   result <- sma_analyse(sims=dat,
                                  code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                  mode=sma_set_mode("quick"))
   
   result4 <- readRDS("Bayesian_results/result4.rds")
   
   expect_equal(result, result4)
   
})


test_that("use r.hat.nodes and ess.nodes",{
   set.seed(100L)
   code = "N[2] ~ dbin(1-phi, N.1)
      for(t in 3:K){
         N[t] ~ dbin(1-phi, N[t-1])
      }
      Y[1] ~ dnorm(N.1, 1/sigma^2)
      for(t in 2:K){
         Y[t] ~ dnorm(N[t], 1/sigma^2)
      }
      sigma2 <- sigma^2"
   sims <- sims::sims_simulate(code=code,
                               constants=nlist(K=4),
                               parameters=nlist(phi=0.95, 
                                                N.1=100,
                                                sigma=5),
                               latent=NA, stochastic = NA, monitor=c("Y"))
   result <- sma_analyse(sims=sims,
                                  code = code,
                                  code.add = "phi ~ dunif(0,1) \n sigma ~ dunif(0,20) \n N.1 ~ dpois(100)",
                                  mode=sma_set_mode("quick", r.hat.nodes="phi", ess.nodes = "phi"))
   
   result5 <- readRDS("Bayesian_results/result5.rds")
   
   expect_equal(result, result5)
   
})


test_that("lists instead of nlists in simanalyse",{
   set.seed(10L)
   params <- list(mu=0)
   dat <- sims_simulate("a ~ dnorm(mu,10) \n b = 10 %*% 2", parameters = params, nsims=2)
   data.list <- list(list(a=dat[[1]]$a), list(a=dat[[2]]$a))
   result <- sma_analyse(sims=data.list,
                                  code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                  mode=sma_set_mode("quick"))
   
   result6 <- readRDS("Bayesian_results/result6.rds")
   
   expect_equal(result, result6)
   
})
