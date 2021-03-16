library(testthat)
library(simanalyse)

context("function-summarise")

test_that("sma_summarise works", {
  set.seed(10L)
  code <- "for(i in 1:10){x[i] ~ dnorm(0,1/variance)}"
  parameters = nlist(variance=4)
  dat <- sims::sims_simulate(code, parameters = parameters, nsims=2)
  res <- sma_analyse(dat, code, code.add = "variance ~ dunif(0,10)", 
                              mode=sma_set_mode("quick"), monitor="variance",
                              deviance=FALSE)
  summary.res <- sma_summarise(res, measures = "mean")
  
  result1 <- readRDS("Summarise_results/result1.rds")
  
  expect_identical(summary.res, result1) 
  
  summary.res2 <- sma_summarise(res, 
                                measures = "", 
                                custom_funs = list("mean"=mean),
                                monitor="variance")
  
  expect_identical(summary.res, summary.res2)
})

