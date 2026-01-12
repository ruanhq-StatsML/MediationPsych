\name{deviance_r2_es}
\alias{deviance_r2_es}
\title{Deviance pseudo-R2 effect size}
\description{Computes a pseudo-R2 analogue using pscl::pR2 on GLM fits.}
\usage{
deviance_r2_es(X, M, Y, family = stats::gaussian())
}
\arguments{
  \item{X}{Numeric vector exposure.}
  \item{M}{Numeric matrix of mediators.}
  \item{Y}{Numeric vector response.}
  \item{family}{A stats::family object passed to stats::glm.}
}
\value{A numeric scalar effect size.}
\examples{
if (requireNamespace('pscl', quietly = TRUE)) {
  set.seed(1)
  n <- 200
  X <- rnorm(n)
  M <- matrix(rnorm(n*3), n, 3)
  Y <- 0.2*X + M %*% c(0.1,0.2,0) + rnorm(n)
  deviance_r2_es(X, M, Y)
} else {
  message('Install pscl to run this example.')
}
}
