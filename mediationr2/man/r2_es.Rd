\name{r2_es}
\alias{r2_es}
\title{R2 effect size for mediation}
\description{Computes R2(X,Y) + R2(M,Y) - R2(X,M,Y) for each response dimension.}
\usage{
r2_es(X, M, Y)
}
\arguments{
  \item{X}{Numeric vector exposure.}
  \item{M}{Numeric matrix of mediators.}
  \item{Y}{Numeric matrix or vector of responses.}
}
\value{A numeric vector of length ncol(Y) (or length 1 if Y is a vector).}
\examples{
set.seed(1)
X <- rnorm(100)
M <- matrix(rnorm(100*2), 100, 2)
Y <- 0.2*X + M %*% c(0.1, 0.0) + rnorm(100)
r2_es(X, M, Y)
}
