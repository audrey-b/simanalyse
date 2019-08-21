context("analyse-bayesian")

test_that("sma_analyse_bayesian",{
  set.seed(10L)
  params <- nlist(mu=0)
  dat <- sims_simulate("a ~ dnorm(mu,1)", parameters = params, nsims=2)
  result <- sma_analyse_bayesian(sims=dat,
                                 code = "a ~ dnorm(mu,1)
                                         mu ~ dunif(-3,3)",
                                 n.adapt = 101,
                                 n.burnin = 0,
                                 n.iter = 101,
                                 monitor = "mu")
  expect_true(class(result)=="mcmcrs")
  expect_equal(result[[1]]$mu[1], -1.817165, tolerance = 0.000001)
  
  #assess_within(result, aggregate_FUN=var)
  
  #sma_assess(result, "Epsd", parameters=params, monitor="mu")
})

test_that("save analyses to files",{
  tempdir <- file.path(tempdir(), "sims")
  unlink(tempdir, recursive = TRUE)
  set.seed(10L)
  params <- nlist(mu=0)
  sims_simulate("a ~ dnorm(mu,1)", 
                parameters = params, 
                nsims=2,
                path=tempdir)
  sims_data_files(tempdir)
  
  sma_analyse_bayesian(code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                       n.adapt = 101,
                       n.burnin = 0,
                       n.iter = 101,
                       monitor = "mu",
                       path.read = tempdir,
                       path.save = file.path(tempdir, "analyses"))
  
})


 test_that("monitor is a list of inits for multiple chains",{
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
                                  n.adapt = 101,
                                  n.burnin = 0,
                                  n.iter = 1,
                                  n.chains=3,
                                  monitor = c("mu", "tt"),
                                  inits=list(inits1, inits2, inits3))
 })


