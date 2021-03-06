% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simanalyse-analyse.R
\name{sma_analyse_files}
\alias{sma_analyse_files}
\title{Analyse data for a simulation study}
\usage{
sma_analyse_files(
  code,
  code.add = "",
  code.values = NULL,
  monitor = ".*",
  inits = list(),
  mode = sma_set_mode("report"),
  deviance = TRUE,
  path = ".",
  folder = "analysis0000001",
  progress = FALSE,
  options = furrr::furrr_options(seed = TRUE)
)
}
\arguments{
\item{code}{A string of code to analyze the data. JAGS code must not be in a data or model block.}

\item{code.add}{A string of code to add at the end of \code{code} before analysing the data. This is useful for adding priors to the likelihood.}

\item{code.values}{A character vector to replace all instances of "?" in the model. This is useful for varying choices of distributions, e.g. for assessing sensitivity to the choice of priors.}

\item{monitor}{A character vector (or regular expression if a string) specifying the names of the stochastic nodes to output from the analysis. By default all stochastic nodes are included.}

\item{inits}{A list or a function. Initial values for the MCMC chains. If specifying a function, it should either have no arguments, or have a single argument named chain. In the latter case, the supplied function is called with the chain number as argument. In this way, initial values may be generated that depend systematically on the chain number.}

\item{mode}{A list obtained from sma_set_mode which sets the parameters of the mcmc sampling.}

\item{deviance}{A flag. Indicates whether to monitor deviance for future DIC calculation.}

\item{path}{A string. Sims are read from that path on disk and results are written to disk.}

\item{folder}{A string for the name of the folder to save the results.}

\item{progress}{A flag specifying whether to print a progress bar.}

\item{options}{The future specific options to use with the workers.}
}
\value{
A flag.
}
\description{
Analyse data for a simulation study. Allows data to be read from files and results to be written to files.
}
\examples{
 set.seed(10L)
 code <- "a ~ dnorm(mu,1)"
 sims <- sims::sims_simulate(code, parameters = nlist(mu=0), nsims=2)
 prior = "mu ~ dunif(-3,3)"
 result <- sma_analyse(sims=sims,
                                       code = code,
                                       code.add = prior,
                                       mode=sma_set_mode("quick"),
                                       monitor = "mu")
}
