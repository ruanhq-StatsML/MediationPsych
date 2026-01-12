\name{simulate_Z}
\alias{simulate_Z}
\title{Simulate mixed covariates Z}
\description{Combines continuous and binary covariates. Continuous generation requires suggested packages.}
\usage{
simulate_Z(n, n_Z, n_cont)
}
\arguments{
  \item{n}{Sample size.}
  \item{n_Z}{Total number of covariates.}
  \item{n_cont}{Number of continuous covariates (0..n_Z).}
}
\value{An n x n_Z numeric matrix.}
\examples{
set.seed(1)
Z <- simulate_Z(10, n_Z = 3, n_cont = 0)
dim(Z)
}
