
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simanalyse

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/audrey-b/simanalyse/workflows/R-CMD-check/badge.svg)](https://github.com/audrey-b/simanalyse/actions)
[![Codecov test
coverage](https://github.com/audrey-b/simanalyse/workflows/test-coverage/badge.svg)](https://codecov.io/gh/audrey-b/simanalyse?branch=master)
[![License:
GPL3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/simanalyse)](https://cran.r-project.org/package=simanalyse)
![CRAN downloads](http://cranlogs.r-pkg.org/badges/simanalyse)
<!-- badges: end -->

simanalyse is an R package to facilitate model comparisons and
simulation studies.

To install the latest development version from
[GitHub](https://github.com/audrey-b/simanalyse)

``` r
#install.packages("remotes")
remotes::install_github("audrey-b/simanalyse")
```

## Demonstration

### Simulate Data

Simulate 5 datasets using the sims package (here we use only a small
number of datasets for the sake of illustration).

``` r
library(simanalyse)
#> Registered S3 method overwritten by 'rjags':
#>   method               from 
#>   as.mcmc.list.mcarray mcmcr
set.seed(10L)
params <- list(sigma = 2)
constants <- list(mu = 0)
code <- "for(i in 1:10){
          y[i] ~ dnorm(mu, 1/sigma^2)}"
sims <- sims::sims_simulate(code, 
                            parameters = params, 
                            constants = constants,
                            nsims = 5,
                            silent = TRUE)
print(sims)
#> $y
#>  [1]  1.29495655 -0.63919833  0.07602842 -1.55116546  0.89066792 -0.82298676
#>  [7]  1.03237779  1.69966601 -1.33777215 -0.77232720
#> 
#> $mu
#> [1] 0
#> 
#> an nlists object of 5 nlist objects each with 2 numeric elements
```

### Analyse Data

Analyse each of the dataset in “report” mode. By default, this mode runs
3 chains until the following two criteria are met: convergence based on
r.hat &lt;1.1 and a minimum effective sample size of 400. The chains are
thinned to 4000 iterations to preserve disk and memory usage. See
?sma\_set\_mode for other choices of analysis mode and customization.

``` r
prior <- "sigma ~ dunif(0, 6)"
results <- sma_analyse(sims = sims,
                       code = code,
                       code.add = prior,
                       mode = sma_set_mode("report"))
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
#> 
#> Max r.hat= 1.001
#> Min ess= 2772
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
#> 
#> Max r.hat= 1.001
#> Min ess= 3516
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
#> 
#> Max r.hat= 1
#> Min ess= 3552
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
#> 
#> Max r.hat= 1
#> Min ess= 3120
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
#> 
#> Max r.hat= 1
#> Min ess= 4380
#> Module dic unloaded
```

### Derive new parameters (if required)

Derive posterior samples for new parameters.

``` r
results.derived <- sma_derive(results, "var=sigma^2", monitor="var")
print(results.derived)
#> $mcmcr1
#> $var
#> [1] 2.535958
#> 
#> nchains:  3 
#> niters:  4000 
#> 
#> 
#> $mcmcr2
#> $var
#> [1] 3.34022
#> 
#> nchains:  3 
#> niters:  4000 
#> 
#> 
#> $mcmcr3
#> $var
#> [1] 5.490214
#> 
#> nchains:  3 
#> niters:  4000 
#> 
#> 
#> $mcmcr4
#> $var
#> [1] 2.534139
#> 
#> nchains:  3 
#> niters:  4000 
#> 
#> 
#> $mcmcr5
#> $var
#> [1] 7.283483
#> 
#> nchains:  3 
#> niters:  4000
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
#> an nlist object with 1 numeric element
```

### Summarise the results of the simulation study

Evaluate the performance of the model using the 5 analyses

``` r
sma_evaluate(results.derived, parameters=params.derived)
#>   term     bias cpQuantile      mse
#> 1  var 1.092141          1 6.162797
```

Several more performance measures are available and can be specified
using the *measures* argument (see ?sma\_evaluate for details). You may
also create custom performance measures. The example below shows how to
reproduce the results above with custom code.

``` r
sma_evaluate(results.derived,
             measures = "", 
             parameters = params.derived, 
             custom_funs = list(estimator = mean,
                                cp.low = function(x) quantile(x, 0.025),
                                cp.upp = function(x) quantile(x, 0.975)),
             custom_expr_b = "bias = estimator - parameters
                              mse = (estimator - parameters)^2
                              cpQuantile = ifelse((parameters >= cp.low) & (parameters <= cp.upp), 1, 0)")
#>   term     bias cpQuantile      mse
#> 1  var 1.092141          1 6.162797
```

## Saving to file

When running simulation studies, it is often preferable to save all the
results to disk. By default, when the *path* argument is not specified,
results are saved in your working directory.

``` r
set.seed(10L)
sims::sims_simulate(code, 
                    parameters = params, 
                    constants = constants,
                    nsims = 5,
                    save=TRUE,
                    exists = NA)
#> [1] TRUE

sma_analyse_files(code = code,
                  code.add = prior,
                  mode = sma_set_mode("report"))
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
#> 
#> Max r.hat= 1.001
#> Min ess= 2772
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
#> 
#> Max r.hat= 1.001
#> Min ess= 3516
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
#> 
#> Max r.hat= 1
#> Min ess= 3552
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
#> 
#> Max r.hat= 1
#> Min ess= 3120
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
#> 
#> Max r.hat= 1
#> Min ess= 4380
#> v data0000001.rds [00:00:00.298]
#> v data0000002.rds [00:00:00.277]
#> v data0000003.rds [00:00:00.293]
#> v data0000004.rds [00:00:00.285]
#> v data0000005.rds [00:00:00.311]
#> Success: 5
#> Failure: 0
#> Remaining: 0
#> 
#> Module dic unloaded

sma_derive_files(code="var=sigma^2", monitor="var")
#> v results0000001.rds [00:00:01.055]
#> v results0000002.rds [00:00:01.184]
#> v results0000003.rds [00:00:01.284]
#> v results0000004.rds [00:00:01.066]
#> v results0000005.rds [00:00:00.951]
#> Success: 5
#> Failure: 0
#> Remaining: 0
#> 

sma_evaluate_files()
```

You may show the files created with

``` r
files <- list.files(getwd(), recursive=TRUE, all.files=TRUE)
print(files)
#>  [1] ".sims.rds"                                  
#>  [2] "analysis0000001/.seeds.rds"                 
#>  [3] "analysis0000001/derived/.parameters.rds"    
#>  [4] "analysis0000001/derived/deriv0000001.rds"   
#>  [5] "analysis0000001/derived/deriv0000002.rds"   
#>  [6] "analysis0000001/derived/deriv0000003.rds"   
#>  [7] "analysis0000001/derived/deriv0000004.rds"   
#>  [8] "analysis0000001/derived/deriv0000005.rds"   
#>  [9] "analysis0000001/performance/performance.rds"
#> [10] "analysis0000001/results/results0000001.rds" 
#> [11] "analysis0000001/results/results0000002.rds" 
#> [12] "analysis0000001/results/results0000003.rds" 
#> [13] "analysis0000001/results/results0000004.rds" 
#> [14] "analysis0000001/results/results0000005.rds" 
#> [15] "data0000001.rds"                            
#> [16] "data0000002.rds"                            
#> [17] "data0000003.rds"                            
#> [18] "data0000004.rds"                            
#> [19] "data0000005.rds"
```

and read a particular file, e.g.

``` r
readRDS(file.path(getwd(), files[9]))
#>   term     bias      mse cpQuantile
#> 1  var 1.092141 6.162797          1
```

## Parallelization

Parallelization is achieved using the
[future](https://github.com/HenrikBengtsson/future) package.

To use all available cores on the local machine simply execute the
following code before calling any of the package’s functions.

library(future) plan(multisession)

## Contribution

Please report any
[issues](https://github.com/audrey-b/simanalyse/issues).

[Pull requests](https://github.com/audrey-b/simanalyse/pulls) are always
welcome.

## Code of Conduct

Please note that the simanalyse project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
