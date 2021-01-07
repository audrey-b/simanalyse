library(testthat)
library(simanalyse)

test_that("work with files",{
  tempdir <- file.path(tempdir(), "sims12")
  unlink(tempdir, recursive = TRUE)
  dir.create(tempdir)
  set.seed(10L)
  params <- nlist(mu=0)
  sims_simulate("a ~ dnorm(mu,1)", 
                parameters = params, 
                nsims=2,
                path=tempdir,
                save=TRUE,
                exists=NA)
  sims_data_files(tempdir)
  sma_analyse(code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                       mode=sma_set_mode("quick"),
                       monitor = "mu",
                       path = tempdir,
                       deviance=FALSE)
  sma_derive(code="mu2=mu^2", path=tempdir)
  sma_evaluate(path=tempdir)
  res1 <- readRDS(file.path(tempdir, "analysis0000001", "performance", "performance.rds"))
  
  #compare to without files
  set.seed(10L)
  params <- nlist(mu=0)
  sims <- sims_simulate("a ~ dnorm(mu,1)", 
                parameters = params, 
                nsims=2,
                path=tempdir,
                exists=NA)
  res <- sma_analyse(sims=sims,
                       code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                       mode=sma_set_mode("quick"),
                       monitor = "mu",
                       deviance=FALSE)
  deriv <- sma_derive(res, code="mu2=mu^2")
  deriv.params <- sma_derive(params, code="mu2=mu^2")
  res2 <- sma_evaluate(deriv, parameters = deriv.params)
  
  #identical(res1, res2)
  
  #compare to parallel with files
  library(future)
  plan(multisession)
  tempdir2 <- file.path(tempdir(), "sims2")
  unlink(tempdir2, recursive = TRUE)
  dir.create(tempdir2)
  set.seed(10L)
  params <- nlist(mu=0)
  sims_simulate("a ~ dnorm(mu,1)", 
                parameters = params, 
                nsims=2,
                path=tempdir2,
                save=TRUE,
                exists=NA)
  sims_data_files(tempdir2)
  sma_analyse(code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                       mode=sma_set_mode("quick"),
                       monitor = "mu",
                       path = tempdir2,
                       deviance=FALSE)
  sma_derive(code="mu2=mu^2", path=tempdir2)
  sma_evaluate(path=tempdir2)
  res3 <- readRDS(file.path(tempdir2, "analysis0000001", "performance", "performance.rds"))
  #identical(res1, res3)
  
  #compare to parallel without files
  set.seed(10L)
  params <- nlist(mu=0)
  sims <- sims_simulate("a ~ dnorm(mu,1)", 
                        parameters = params, 
                        nsims=2,
                        path=tempdir,
                        exists=NA)
  res <- sma_analyse(sims=sims,
                              code = "a ~ dnorm(mu,1)
                               mu ~ dunif(-3,3)",
                              mode=sma_set_mode("quick"),
                              monitor = "mu",
                              deviance=FALSE)
  deriv <- sma_derive(res, code="mu2=mu^2")
  deriv.params <- sma_derive(params, code="mu2=mu^2")
  res4 <- sma_evaluate(deriv, parameters = deriv.params)
  
  #identical(res1, res4)
})