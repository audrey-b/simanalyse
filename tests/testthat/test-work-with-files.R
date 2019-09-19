test_that("work with files",{
  tempdir <- file.path(tempdir(), "sims")
  unlink(tempdir, recursive = TRUE)
  set.seed(10L)
  params <- nlist(mu=0)
  sims_simulate("a ~ dnorm(mu,1)", 
                parameters = params, 
                nsims=2,
                path=tempdir,
                exists=NA)
  sims_data_files(tempdir)
  
  sma_analyse_bayesian(code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                       n.adapt = 101,
                       n.burnin = 0,
                       n.iter = 101,
                       monitor = "mu",
                       path = tempdir,
                       seed=56L,
                       deviance=FALSE)
  
  sma_derive(code="mu2=mu^2", path=tempdir)
  
})