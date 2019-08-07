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

