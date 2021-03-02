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
   expect_true(class(result)=="mcmcrs")
   #expect_true(niters(result)==100)
   #expect_equal(result[[1]]$mu[1], -1.817165, tolerance = 0.000001)
   
   #evaluate_within(result, aggregate_FUN=var)
   
   #sma_evaluate(result, "Epsd", parameters=params, monitor="mu")
})




#test_that("inits is a list of inits for multiple chains",{
#   set.seed(10L)
#   code <- "a ~ dnorm(mu,1)"
#   sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
#   prior = "mu ~ dunif(-3,3)
#           tt ~ dnorm(0,1)"
#   inits1 <- list("mu"=-2, "tt"=0)
#   inits2 <- list("mu"=-2, "tt"=0)
#   inits3 <- list("mu"=-2, "tt"=0)
#   result <- sma_analyse(sims=sims,
#                                  code = code,
#                                  code.add = prior,
#                                  mode=sma_set_mode("quick", n.chains=3),
#                                  monitor = c("mu", "tt"),
#                                  inits=list(inits1, inits2, inits3),
#                                  deviance=FALSE)
#})


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
   result1 <- sma_analyse(sims=sims,
                                   code = code,
                                   code.add = prior,
                                   mode=sma_set_mode("quick"),
                                   monitor = c("mu", "tt"),
                                   inits=inits.fun,
                                   deviance=FALSE)
   set.seed(99L)
   result2 <- sma_analyse(sims=sims,
                                   code = code,
                                   code.add = prior,
                                   mode=sma_set_mode("quick"),
                                   monitor = c("mu", "tt"),
                                   inits=inits.fun,
                                   deviance=FALSE)
   expect_identical(result1, result2)
})

test_that("modulo_in_code and default monitor",{
   set.seed(10L)
   params <- nlist(mu=0)
   dat <- sims_simulate("a ~ dnorm(mu,10) \n b = 10 %*% 2", parameters = params, nsims=2)
   result <- sma_analyse(sims=dat,
                                  code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                  mode=sma_set_mode("quick"))
   expect_true(class(result)=="mcmcrs")
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
                                  mode=sma_set_mode("report", r.hat.nodes="phi", ess.nodes = "phi", n.save=250))
   expect_true(class(result)=="mcmcrs")
   expect_true(mcmcr::ess(result, by="term")[[1]]$phi==535)
   expect_true(mcmcr::rhat(result, by="term")[[1]]$phi==1)
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
   expect_true(class(result)=="mcmcrs")
})
