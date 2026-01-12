\name{deviance_r2_pma}
\alias{deviance_r2_pma}
\title{Bootstrap power analysis for GLM mediation (deviance pseudo-R2)}
\description{Monte Carlo power estimation using bootstrap confidence intervals on deviance_r2_es; requires pscl.}
\usage{
deviance_r2_pma(n, p_M, p_Y = 1, B = 500, errorM = 1, errorY = 1,
  mean_tau, error_tau = 1e-4, n_mc = 500, mag_XM, mag_MY,
  alpha_level = 0.05, family = stats::gaussian())
}
\arguments{
  \item{n}{Sample size.}
  \item{p_M}{Number of mediators.}
  \item{p_Y}{Number of response dimensions.}
  \item{B}{Bootstrap replicates.}
  \item{errorM}{Mediator noise SD.}
  \item{errorY}{Response noise SD.}
  \item{mean_tau}{Mean direct effect.}
  \item{error_tau}{Direct effect noise SD.}
  \item{n_mc}{Monte Carlo repetitions.}
  \item{mag_XM}{Magnitude labels for X->M.}
  \item{mag_MY}{Magnitude labels for M->Y.}
  \item{alpha_level}{Significance level.}
  \item{family}{GLM family passed to deviance_r2_es.}
}
\value{A numeric vector of length p_Y giving estimated power per response dimension.}
\examples{
if (requireNamespace('pscl', quietly = TRUE)) {
  set.seed(1)
  res <- deviance_r2_pma(
    n = 80, p_M = 3, p_Y = 1, B = 30, n_mc = 10,
    errorM = 1, errorY = 1, mean_tau = 0.14,
    mag_XM = c(1,1,1), mag_MY = c(2,2,2)
  )
  res
} else {
  message('Install pscl to run this example.')
}
}
