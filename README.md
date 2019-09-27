
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

To install the latest development version from
[GitHub](https://github.com/audrey-b/simanalyse)

``` r
#install.packages("remotes")
remotes::install_github("audrey-b/simanalyse")
```

## Demonstration

### Simulate Data

Simulate 3 datasets using the sims package (we use only 3 datasets for
demonstration purposes)

``` r
library(simanalyse)
#> Loading required package: nlist
#> Registered S3 method overwritten by 'rjags':
#>   method               from 
#>   as.mcmc.list.mcarray mcmcr
set.seed(10L)
future::plan("sequential")
params <- nlist(sigma = 2)
constants <- nlist(mu = 0)
code <- "for(i in 1:10){
          a[i] ~ dnorm(mu, 1/sigma^2)}"
sims <- sims::sims_simulate(code, 
                           parameters = params, 
                           constants = constants,
                           nsims = 3,
                           silent = TRUE)
print(sims)
#> $a
#>  [1] -2.54871809 -0.82385556 -0.41040854  0.07698506  2.14864217
#>  [6] -0.29359147 -1.24805791  0.07712952 -1.09009556 -0.41968340
#> 
#> $mu
#> [1] 0
#> 
#> an nlists object of 3 nlist objects each with 2 natomic elements
```

### Analyse Data

Analyse all 3 datasets (here we use only a few iterations for
demonstration purposes)

``` r
prior <- "sigma ~ dunif(0, 6)"
results <- sma_analyse_bayesian(sims = sims,
                                code = code,
                                code.add = prior,
                                n.adapt = 100,
                                n.burnin = 0,
                                n.iter = 1000,
                                monitor = names(params))
#> module dic loaded
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
#> 
#> Module dic unloaded
```

### Derive new parameters (if required)

Derive posterior samples for new parameters.

``` r
results.derived <- sma_derive(results, "var=sigma^2", monitor="var")
#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.

#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.

#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.
print(results.derived)
#> $mcmcr1
#> $var
#> [1] 6.214758
#> 
#> nchains:  3 
#> niters:  1000 
#> 
#> 
#> $mcmcr2
#> $var
#> [1] 4.837388
#> 
#> nchains:  3 
#> niters:  1000 
#> 
#> 
#> $mcmcr3
#> $var
#> [1] 3.92309
#> 
#> nchains:  3 
#> niters:  1000
```

The same transformation must be applied to the true parameter values for
eventually evaluating the performance (e.g. bias) of the method for
those new parameters,

``` r
params.derived <- sma_derive(params, "var=sigma^2", monitor="var")
print(params.derived)
#> $var
#> [1] 4
#> 
#> an nlist object with 1 natomic element
```

### Summarise the results of the simulation study

Evaluate the performance of the model using the 3 analyses

``` r
sma_evaluate(results.derived, parameters=params.derived)
#> $bias.var
#> [1] 1.826858
#> 
#> $cp.quantile.var
#> [1] 1
#> 
#> $mse.var
#> [1] 4.443817
#> 
#> an nlist object with 3 natomic elements
```

You may also create custom performance measures. The example below shows
how to reproduce the results above with custom code.

``` r
sma_evaluate(results.derived,
              measures = "", 
              parameters = params.derived, 
              custom_funs = list(estimator = mean,
                                 cp.low = function(x) quantile(x, 0.025),
                                 cp.upp = function(x) quantile(x, 0.975)),
              custom_expr_b = "bias = estimator - parameters
                              mse = (estimator - parameters)^2
                              cp.quantile = ifelse((parameters >= cp.low) & (parameters <= cp.upp), 1, 0)")
#> $bias.var
#> [1] 1.826858
#> 
#> $cp.quantile.var
#> [1] 1
#> 
#> $mse.var
#> [1] 4.443817
#> 
#> an nlist object with 3 natomic elements
```

## Saving to file

When running simulation studies, it is often preferable to work from
disk to keep a copy of all the results. You may save results to file by
specifying a path argument in functions, or more simply by specifying
the path using options(sims.path =), as follows:

``` r
set.seed(10L)
future::plan("sequential")
options(sims.path = file.path(tempdir(), "sims"))

sims::sims_simulate(code, 
                    parameters = params, 
                    constants = constants,
                    nsims = 3,
                    exists = NA)
#> [1] TRUE

sma_analyse_bayesian(code = code,
                     code.add = prior,
                     n.adapt = 101,
                     n.burnin = 0,
                     n.iter = 3,
                     monitor = names(params))
#> module dic loaded
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
#> 
#> Module dic unloaded

sma_derive(code="var=sigma^2", monitor="var")
#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.

#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.

#> Warning: The following parameters were not in expr and so were dropped from
#> object: 'deviance'.

sma_evaluate()
```

You may show the files created with

``` r
files <- list.files(getOption("sims.path"), recursive=TRUE, all.files=TRUE)
print(files)
#>  [1] ".sims.rds"                                 
#>  [2] "analysis0000001/.seeds.rds"                
#>  [3] "analysis0000001/derived/.parameters.rds"   
#>  [4] "analysis0000001/derived/deriv0000001.rds"  
#>  [5] "analysis0000001/derived/deriv0000002.rds"  
#>  [6] "analysis0000001/derived/deriv0000003.rds"  
#>  [7] "analysis0000001/results/results0000001.rds"
#>  [8] "analysis0000001/results/results0000002.rds"
#>  [9] "analysis0000001/results/results0000003.rds"
#> [10] "data0000001.rds"                           
#> [11] "data0000002.rds"                           
#> [12] "data0000003.rds"                           
#> [13] "performance/performance.rds"
```

or read a particular file, e.g.

``` r
readRDS(file.path(getOption("sims.path"), files[13]))
#> $bias.var
#> [1] 4.782415
#> 
#> $cp.quantile.var
#> [1] 1
#> 
#> $mse.var
#> [1] 69.04043
#> 
#> an nlist object with 3 natomic elements
```

Here there is a discrepency with before because the seed argument as not
been incorporated properly yet.

## Parallelization

Parallelization is achieved using the
[future](https://github.com/HenrikBengtsson/future) package.

To use all available cores on the local machine simply execute the
following code before calling `sims_analyse_bayesian()`,
`sims_analyse_derive()` and/or `sims_analyse_evaluate()`.

    library(future)
    plan(multisession)

## Contribution

Please report any
[issues](https://github.com/audrey-b/simanalyse/issues).

[Pull requests](https://github.com/audrey-b/simanalyse/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/audrey-b/simanalyse/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
