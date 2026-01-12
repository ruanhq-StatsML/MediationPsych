\name{simulate_Z_cont}
\alias{simulate_Z_cont}
\title{Simulate continuous covariates Z}
\description{Generates non-normal correlated covariates using suggested packages simstudy, corpcor, and semTools.}
\usage{
simulate_Z_cont(n, n_Z)
}
\arguments{
  \item{n}{Sample size.}
  \item{n_Z}{Number of continuous covariates.}
}
\value{An n x n_Z numeric matrix.}
\examples{
if (requireNamespace('simstudy', quietly = TRUE) && requireNamespace('corpcor', quietly = TRUE) && requireNamespace('semTools', quietly = TRUE)) {
  set.seed(1)
  Z <- simulate_Z_cont(50, 4)
  dim(Z)
} else {
  message('Install simstudy, corpcor, semTools to run this example.')
}
}
