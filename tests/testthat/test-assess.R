
test_that("sma_assess",{
  set.seed(10L)
  code="for(i in 1:10){
  a[i] ~ dpois(mu)}"
  sims <- sims::sims_simulate(code, parameters=nlist(mu=5), nsims=1)
  res <- sma_analyse_bayesian(sims, 
                              code, 
                              "mu ~ dunif(0,10)", 
                              monitor = "mu",
                              mode=sma_set_mode("debug"),
                              deviance=FALSE)
  sma_assess(res, "a = mu", sims, code, nsamples=5L)
})


