
test_that("sma_assess",{
  set.seed(10L)
  code="for(i in 1:10){
  a[i] ~ dpois(mu)}"
  sims <- sims::sims_simulate(code, parameters=nlist(mu=5), nsims=1)
  res <- sma_analyse_bayesian(sims, 
                            code, 
                              "mu ~ dunif(0,10)", 
                              monitor = "mu",
                              n.adapt=100,
                              n.burnin=0,
                              n.iter=50)
   sma_assess(res, "a = mu", sims, code, nsamples=5)
})


