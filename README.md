
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
summarise the results.

## Installation

To install the latest release version from
[CRAN](https://cran.r-project.org)

``` r
install.packages("simanalyse")
```

To install the latest development version from
[GitHub](https://github.com/audrey-b/simanalyse)

``` r
# install.packages("remotes")
remotes::install_github("audrey-b/simanalyse")
```

## Demonstration

In order to create a new package the user should

1)  Go to the simanalyse [GitHub
    repository](https://github.com/audrey-b/simanalyse) and choose ‘Use
    this template’.
2)  Clone the new repository and replace ‘simanalyse’ with the name of
    the new package in `DESCRIPTION`, `NEWS.md`, `tests/testthat.R` and
    this `README.Rmd` file.
3)  `devtools::check()` the package and fix any Errors, Warnings or
    Notes.
4)  Knit this `README.Rmd` file and `pkgdown::build_site()`.
5)  Add the project to
    [Travis](https://www.travis-ci.com/audrey-b/simanalyse) and in the
    [Settings](https://www.travis-ci.com/audrey-b/simanalyse/settings)
    add a Cron Job to run the master branch daily if there hasn’t been a
    build in the last 24h.
6)  Add the project to
    [Appveyor](https://ci.appveyor.com/project/audrey-b/simanalyse).
7)  Rename the `simanalyse.Rproj` file.
8)  Push the changes to the new repository.
9)  Go to the repository GitHub
    [settings](https://github.com/audrey-b/simanalyse/settings) and set
    the GitHub Pages Source to be the master branch /docs folder.
10) Edit the GitHub repository
    [description](https://github.com/audrey-b/simanalyse/) and set the
    website to be <https://audrey-b.github.io/simanalyse/>.

## Contribution

Please report any
[issues](https://github.com/audrey-b/simanalyse/issues).

[Pull requests](https://github.com/audrey-b/simanalyse/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/audrey-b/simanalyse/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
