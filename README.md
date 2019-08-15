
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
params <- nlist(mu = 0)
constants <- nlist(sigma = 1)
code <- "for(i in 1:10){
          a[i] ~ dnorm(mu, 1/sigma^2)}"
dat <- sims::sims_simulate(code, 
                           parameters = params, 
                           constants = constants,
                           nsims = 3,
                           silent = TRUE)
print(dat)
#> $a
#>  [1] -1.27435905 -0.41192778 -0.20520427  0.03849253  1.07432109
#>  [6] -0.14679574 -0.62402896  0.03856476 -0.54504778 -0.20984170
#> 
#> $sigma
#> [1] 1
#> 
#> an nlists object of 3 nlist objects each with 2 natomic elements
```

Analyse all 3 datasets (here we use only a few iterations for
demonstration purposes)

``` r
prior <- "mu ~ dunif(-3,3)"
result <- sma_analyse_bayesian(data = dat,
                               code = code,
                               code.add = prior,
                               n.adapt = 100,
                               n.burnin = 0,
                               n.iter = 3,
                               monitor = "mu")
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

Summarize the results over the 3 datasets

``` r
sma_summarise(result, parameters=params)
#> $bias.mu
#> [1] -0.09627386
#> 
#> $cp.quantile.mu
#> [1] 1
#> 
#> $mse.mu
#> [1] 0.02914233
#> 
#> an nlist object with 3 natomic elements
```

You may also create customized Monte Carlo measures. The example below
shows how to reproduce the results above with custom code.

``` r
sma_summarise(result,
              measures = "", 
              parameters = params, 
              custom_FUNS = list(estimator = mean,
                                 cp.low = function(x) quantile(x, 0.025),
                                 cp.high = function(x) quantile(x, 0.975)),
              custom_expr_b = "bias = estimator - parameters
                              mse = (estimator - parameters)^2
                              cp.quantile = ifelse((parameters >= cp.low) & (parameters <= cp.high), 1, 0)")
#> $bias.mu
#> [1] -0.09627386
#> 
#> $cp.quantile.mu
#> [1] 1
#> 
#> $mse.mu
#> [1] 0.02914233
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
                     monitor = "mu",
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
#> SUCCESS 1/3/0 [2019-08-15 18:43:07] 'data0000001.rds'
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
#> SUCCESS 2/3/0 [2019-08-15 18:43:07] 'data0000002.rds'
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
#> SUCCESS 3/3/0 [2019-08-15 18:43:07] 'data0000003.rds'
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
