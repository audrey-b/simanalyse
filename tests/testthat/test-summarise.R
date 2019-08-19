context("function-summarise")

test_that("sma_summarise works", {
  set.seed(10L)
  code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
  parameters = nlist(variance=4)
  dat <- sims::sims_simulate(code, parameters = parameters, nsims=2)
  res <- sma_analyse_bayesian(dat, code, code.add = "variance ~ dunif(0,10)", 
                              n.adapt=100, n.burnin=0, n.iter=3, monitor="variance")
  summary.res <- sma_summarise(res)
  expect_identical(length(summary.res), 2L) 
  expect_equal(summary.res[[1]]$mean$variance, 4.491454, tolerance=0.00001) 
  expect_equal(summary.res[[2]]$mean$variance, 4.41608, tolerance=0.00001) 
})

