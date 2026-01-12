# GLM mediation power analysis example (requires suggested package pscl)
library(mediationr2)

set.seed(20)

p_M <- 5
p_Y <- 1

mag_zero_X <- rep(0, p_M)
mag_small_X <- c(1,0,1,0,1)
mag_medium_X <- c(2,1,1,1,2)
mag_large_X <- c(2,3,2,3,2)

mag_zero_YM <- rep(0, p_M)
mag_small_YM <- c(1,0,1,0,1)
mag_medium_YM <- rep(2, p_M)
mag_large_YM <- rep(3, p_M)

deviance_r2_pma(
  n = 120, p_M = p_M, p_Y = p_Y, B = 200, errorY = 1, errorM = 1,
  mean_tau = 0.14, n_mc = 50, mag_XM = mag_zero_X, mag_MY = mag_small_YM,
  alpha_level = 0.05
)
