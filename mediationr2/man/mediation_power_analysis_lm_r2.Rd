\name{mediation_power_analysis_lm_r2}
\alias{mediation_power_analysis_lm_r2}
\title{Bootstrap power analysis for linear mediation (R2 effect size)}
\description{Monte Carlo power estimation using bootstrap confidence intervals on r2_es.}
\usage{
mediation_power_analysis_lm_r2(n, p_M, p_Y, B = 500, errorM = 1, errorY = 1, mean_tau,
  error_level = 0.02, n_mc = 500, mag_XM, mag_MY, magnitude,
  alpha_level = 0.05, covariate_adjustment = FALSE, n_Z = 2, sigma_z = 1,
  snr_generation = FALSE, snr = 1)
}
\arguments{
  \item{n}{Sample size.}
  \item{p_M}{Number of mediators.}
  \item{p_Y}{Number of response dimensions.}
  \item{B}{Bootstrap replicates.}
  \item{errorM}{Mediator noise SD.}
  \item{errorY}{Response noise SD.}
  \item{mean_tau}{Mean direct effect (tau).}
  \item{error_level}{Jitter for path coefficients.}
  \item{n_mc}{Monte Carlo repetitions.}
  \item{mag_XM}{Magnitude labels for X->M.}
  \item{mag_MY}{Magnitude labels for M->Y (p_M x p_Y).}
  \item{magnitude}{Unused; kept for compatibility.}
  \item{alpha_level}{Significance level.}
  \item{covariate_adjustment}{Whether to residualize Y using generated Z covariates.}
  \item{n_Z}{Number of Z covariates if covariate_adjustment is TRUE.}
  \item{sigma_z}{Noise SD for Z generation.}
  \item{snr_generation}{Whether to add noise to hit a target SNR.}
  \item{snr}{Target SNR.}
}
\value{A numeric vector of length p_Y giving estimated power per response dimension.}
\examples{
set.seed(1)
res <- mediation_power_analysis_lm_r2(
  n = 80, p_M = 3, p_Y = 1, B = 50, n_mc = 20,
  errorM = 1, errorY = 1, mean_tau = 0.14,
  mag_XM = c(1,1,1), mag_MY = matrix(c(2,2,2), ncol = 1),
  magnitude = NULL
)
res
}
