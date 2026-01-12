\name{simulate_Z_cate}
\alias{simulate_Z_cate}
\title{Simulate binary covariates Z}
\description{Generates binary covariates with a random proportion of ones in each column.}
\usage{
simulate_Z_cate(n, n_Z)
}
\arguments{
  \item{n}{Sample size.}
  \item{n_Z}{Number of binary covariates.}
}
\value{An n x n_Z numeric matrix with entries in {0,1}.}
\examples{
set.seed(1)
Z <- simulate_Z_cate(10, 3)
dim(Z)
unique(as.vector(Z))
}
