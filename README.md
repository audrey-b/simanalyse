
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simanalyse

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/audrey-b/simanalyse.svg?branch=master)](https://travis-ci.com/audrey-b/simanalyse)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/audrey-b/simanalyse?branch=master&svg=true)](https://ci.appveyor.com/project/audrey-b/simanalyse)
[![Codecov test
coverage](https://codecov.io/gh/audrey-b/simanalyse/branch/master/graph/badge.svg)](https://codecov.io/gh/audrey-b/simanalyse?branch=master)
[![License:
GPL3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Tinyverse
status](https://tinyverse.netlify.com/badge/simanalyse)](https://CRAN.R-project.org/package=simanalyse)
[![CRAN
status](https://www.r-pkg.org/badges/version/simanalyse)](https://cran.r-project.org/package=simanalyse)
![CRAN downloads](http://cranlogs.r-pkg.org/badges/simanalyse)
<!-- badges: end -->

simanalyse is an R package to analyse simulation study data and
summarise results.

## Installation

To install the latest release version from
[CRAN](https://cran.r-project.org)

``` r
install.packages("simanalyse")
```

To install the latest development version from
[GitHub](https://github.com/audrey-b/simanalyse)

``` r
#install.packages("remotes")
remotes::install_github("audrey-b/simanalyse")
```

## Demonstration

Simulate 3 datasets using the sims package (we use only 3 datasets for
demonstration purposes)

``` r
library(simanalyse)
#> Loading required package: nlist
#> Registered S3 method overwritten by 'rjags':
#>   method               from 
#>   as.mcmc.list.mcarray mcmcr
set.seed(10L)
params <- nlist(sigma = 2)
constants <- nlist(mu = 0)
code <- "for(i in 1:10){
          a[i] ~ dnorm(mu, 1/sigma^2)}"
dat <- sims::sims_simulate(code, 
                           parameters = params, 
                           constants = constants,
                           nsims = 3,
                           silent = TRUE)
print(dat)
#> $a
#>  [1] -2.54871809 -0.82385556 -0.41040854  0.07698506  2.14864217
#>  [6] -0.29359147 -1.24805791  0.07712952 -1.09009556 -0.41968340
#> 
#> $mu
#> [1] 0
#> 
#> an nlists object of 3 nlist objects each with 2 natomic elements
```

Analyse all 3 datasets (here we use only a few iterations for
demonstration purposes)

``` r
prior <- "sigma ~ dunif(0, 6)"
result <- sma_analyse_bayesian(data = dat,
                               code = code,
                               code.add = prior,
                               n.adapt = 100,
                               n.burnin = 0,
                               n.iter = 3,
                               monitor = "sigma")
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
#> 
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
#> 
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
```

Derive posterior samples for new parameters. To assess the performance
of the method for those new parameters (e.g. bias), the same
transformation must be applied to the true parameter values.

``` r
derived.result <- sma_derive(result, "var=sigma^2", monitor="var")
print(derived.result)
#> $mcmcr1
#> $var
#> [1] 16.5214
#> 
#> nchains:  3 
#> niters:  3 
#> 
#> 
#> $mcmcr2
#> $var
#> [1] 5.690343
#> 
#> nchains:  3 
#> niters:  3 
#> 
#> 
#> $mcmcr3
#> $var
#> [1] 2.819938
#> 
#> nchains:  3 
#> niters:  3
derived.params <- sma_derive(params, "var=sigma^2", monitor="var")
print(derived.params)
#> $var
#> [1] 4
#> 
#> an nlist object with 1 natomic element
```

Evaluate the performance of the model using the 3 analyses

``` r
sma_assess(derived.result, parameters=derived.params)
#> $bias.var
#> [1] 3.630788
#> 
#> $cp.quantile.var
#> [1] 1
#> 
#> $mse.var
#> [1] 34.41747
#> 
#> an nlist object with 3 natomic elements
```

You may also create customized performance measures. The example below
shows how to reproduce the results above with custom code.

``` r
sma_assess(derived.result,
              measures = "", 
              parameters = derived.params, 
              custom_FUNS = list(estimator = mean,
                                 cp.low = function(x) quantile(x, 0.025),
                                 cp.upp = function(x) quantile(x, 0.975)),
              custom_expr_b = "bias = estimator - parameters
                              mse = (estimator - parameters)^2
                              cp.quantile = ifelse((parameters >= cp.low) & (parameters <= cp.upp), 1, 0)")
#> $bias.var
#> [1] 3.630788
#> 
#> $cp.quantile.var
#> [1] 1
#> 
#> $mse.var
#> [1] 34.41747
#> 
#> an nlist object with 3 natomic elements
```

You may also save results to file with

``` r
set.seed(10L)
sims::sims_simulate(code, 
                    parameters = params, 
                    constants = constants,
                    nsims = 3,
                    exists = NA,
                    path = tempdir())
#> [1] TRUE
sma_analyse_bayesian(code = code,
                     code.add = prior,
                     n.adapt = 101,
                     n.burnin = 0,
                     n.iter = 3,
                     monitor = "sigma",
                     path.read = tempdir(),
                     path.save = tempdir())
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
#> 
#> SUCCESS 1/3/0 [2019-08-19 19:03:15] 'data0000001.rds'
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
#> 
#> SUCCESS 2/3/0 [2019-08-19 19:03:15] 'data0000002.rds'
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 10
#>    Unobserved stochastic nodes: 1
#>    Total graph size: 18
#> 
#> Initializing model
#> 
#> SUCCESS 3/3/0 [2019-08-19 19:03:15] 'data0000003.rds'
#> [1] TRUE
```

## Contribution

Please report any
[issues](https://github.com/audrey-b/simanalyse/issues).

[Pull requests](https://github.com/audrey-b/simanalyse/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/audrey-b/simanalyse/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
