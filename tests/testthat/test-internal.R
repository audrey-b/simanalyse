context("internal")

# test_that("function_template returns TRUE by default", {
#   expect_true(function_template())
# })
# 
# test_that("function_template returns FALSE if x = FALSE", {
#   expect_false(function_template(x = FALSE))
# })
# 
# test_that("function_template errors x is not a FLAG", {
#   expect_error(function_template(1), "^`x` must be a flag [(]TRUE or FALSE[)][.]$")
# })
# 
# 

test_that("make_code works",{
  expect_identical(prepare_code("a ~ dnorm(%s,1)", "b ~ dnorm(%s,1)", -1, 1),
                   "model{\n\na ~ dnorm(-1,1) \n b ~ dnorm(1,1)\n\n}")  
})

test_that("add_model_block works",{
  expect_identical(add_model_block("a ~ dnorm(0,1)"), "model{\n\na ~ dnorm(0,1)\n\n}")
})

test_that("analyse_dataset_bayesian works",{
  result <- analyse_dataset_bayesian(code = "a ~ dnorm(0,1)",
                                     n.adapt = 101,
                                     n.burnin = 0,
                                     n.iter = 101,
                                     monitor = "a",
                                     seed = 10)
  expect_true(class(result)=="nlist")
  expect_equal(result[[1]][1], 0.3628946, tolerance = 0.0000001)
})

test_that("analyse_dataset_bayesian with data works",{
  set.seed(10L)
  dat <- sims::sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=2)
  result <- analyse_dataset_bayesian(nlistdata = dat[[1]],
                                     code = "a ~ dnorm(0,1)
                                             mu ~ dnorm(-3, 3)",
                                     n.adapt = 101,
                                     n.burnin = 0,
                                     n.iter = 101,
                                     monitor = "mu",
                                     seed = 10)
  expect_true(class(result)=="nlist")
  expect_equal(result[[1]][1], -2.790483, tolerance = 0.000001)
})

 test_that("simanalyse_analyse_bayesian works",{
   set.seed(10L)
   dat <- sims::sims_simulate("a ~ dnorm(mu,1)", parameters = nlist(mu=0), nsims=2)
   result <- simanalyse_analyse_bayesian(datalist=dat,
                                         code = "a ~ dnorm(mu,1)
                                      mu ~ dunif(-3,3)",
                                         n.adapt = 101,
                                         n.burnin = 0,
                                         n.iter = 101,
                                         monitor = "mu")
   expect_true(class(result)=="nlists")
   expect_equal(result[[1]]$mu[1], 1.750656, tolerance = 0.000001)
 })



