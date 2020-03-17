context("analyse-bayesian")

test_that("sma_analyse_bayesian",{
   set.seed(10L)
   params <- nlist(mu=0)
   dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = params, nsims=2)
   result <- sma_analyse_bayesian(sims=dat,
                                  code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                  mode=sma_set_mode("quick"),
                                  monitor = "mu")
   expect_true(class(result)=="mcmcrs")
   #expect_true(niters(result)==100)
   #expect_equal(result[[1]]$mu[1], -1.817165, tolerance = 0.000001)
   
   #evaluate_within(result, aggregate_FUN=var)
   
   #sma_evaluate(result, "Epsd", parameters=params, monitor="mu")
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
   result <- sma_analyse_bayesian(sims=sims,
                                  code = code,
                                  code.add = prior,
                                  mode=sma_set_mode("quick", n.chains=3),
                                  monitor = c("mu", "tt"),
                                  inits=list(inits1, inits2, inits3),
                                  deviance=FALSE)
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
   result1 <- sma_analyse_bayesian(sims=sims,
                                   code = code,
                                   code.add = prior,
                                   mode=sma_set_mode("quick"),
                                   monitor = c("mu", "tt"),
                                   inits=inits.fun,
                                   deviance=FALSE)
   set.seed(99L)
   result2 <- sma_analyse_bayesian(sims=sims,
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
   result <- sma_analyse_bayesian(sims=dat,
                                  code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                  mode=sma_set_mode("quick"))
   expect_true(class(result)=="mcmcrs")
   #expect_true(niters(result)==100)
   #expect_equal(result[[1]]$mu[1], -1.817165, tolerance = 0.000001)
   
   #evaluate_within(result, aggregate_FUN=var)
   
   #sma_evaluate(result, "Epsd", parameters=params, monitor="mu")
})

