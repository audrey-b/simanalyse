test_that("sma_assess", {
  set.seed(10L)
  code <- "for(i in c(1,3,6,8)){
  a[i] ~ dpois(mu)}"
  sims <- sims::sims_simulate(code, parameters = nlist(mu = 5), nsims = 1)
  res <- sma_analyse(sims,
    code,
    "mu ~ dunif(0,10)",
    monitor = "mu",
    mode = sma_set_mode("quick"),
    deviance = FALSE
  )
  sma_assess(res, "a = rep(mu, 8)", sims, code, nsamples = 5L)
})
