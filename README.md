
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgtemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/audrey-b/pkgtemplate.svg?branch=master)](https://travis-ci.com/audrey-b/pkgtemplate)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/audrey-b/pkgtemplate?branch=master&svg=true)](https://ci.appveyor.com/project/audrey-b/pkgtemplate)
[![Codecov test
coverage](https://codecov.io/gh/audrey-b/pkgtemplate/branch/master/graph/badge.svg)](https://codecov.io/gh/audrey-b/pkgtemplate?branch=master)
[![License:
GPL3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Tinyverse
status](https://tinyverse.netlify.com/badge/pkgtemplate)](https://CRAN.R-project.org/package=pkgtemplate)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgtemplate)](https://cran.r-project.org/package=pkgtemplate)
![CRAN downloads](http://cranlogs.r-pkg.org/badges/pkgtemplate)
<!-- badges: end -->

pkgtemplate provides a template for a new R package.

## Installation

To install the latest release version from
[CRAN](https://cran.r-project.org)

``` r
install.packages("pkgtemplate")
```

To install the latest development version from
[GitHub](https://github.com/audrey-b/pkgtemplate)

``` r
# install.packages("remotes")
remotes::install_github("audrey-b/pkgtemplate")
```

## Demonstration

In order to create a new package the user should

1)  Go to the pkgtemplate [GitHub
    repository](https://github.com/audrey-b/pkgtemplate) and choose ‘Use
    this template’.
2)  Clone the new repository and replace ‘pkgtemplate’ with the name of
    the new package in `DESCRIPTION`, `NEWS.md`, `tests/testthat.R` and
    this `README.Rmd` file.
3)  `devtools::check()` the package and fix any Errors, Warnings or
    Notes.
4)  Knit this `README.Rmd` file and `pkgdown::build_site()`.
5)  Add the project to
    [Travis](https://www.travis-ci.com/audrey-b/pkgtemplate) and in the
    [Settings](https://www.travis-ci.com/audrey-b/pkgtemplate/settings)
    add a Cron Job to run the master branch daily if there hasn’t been a
    build in the last 24h.
6)  Add the project to
    [Appveyor](https://ci.appveyor.com/project/audrey-b/pkgtemplate).
7)  Rename the `pkgtemplate.Rproj` file.
8)  Push the changes to the new repository.
9)  Go to the repository GitHub
    [settings](https://github.com/audrey-b/pkgtemplate/settings) and set
    the GitHub Pages Source to be the master branch /docs folder.
10) Edit the GitHub repository
    [description](https://github.com/audrey-b/pkgtemplate/) and set the
    website to be <https://audrey-b.github.io/pkgtemplate/>.

## Contribution

Please report any
[issues](https://github.com/audrey-b/pkgtemplate/issues).

[Pull requests](https://github.com/audrey-b/pkgtemplate/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/audrey-b/pkgtemplate/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
