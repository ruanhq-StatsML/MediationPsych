# Linear-model mediation power analysis example
library(mediationr2)

set.seed(20)

p_M <- 5
p_Y <- 3

mag_zero_X  <- rep(0, p_M)
mag_small_X <- rep(1, p_M)
mag_medium_X <- rep(2, p_M)
mag_large_X <- rep(3, p_M)

mag_zero_YM <- matrix(0, nrow = p_M, ncol = p_Y)
mag_small_YM <- matrix(c(1,0,1,0,1, 1,0,0,1,1, 0,0,1,1,1), ncol = p_Y)
mag_medium_YM <- matrix(c(2,1,2,1,2, 2,1,1,2,2, 1,1,2,2,2), ncol = p_Y)
mag_large_YM <- matrix(c(3,2,3,2,3, 3,2,2,3,3, 2,2,3,3,3), ncol = p_Y)

# Type-I error (indirect effect ~0) example
mediation_power_analysis_lm_r2(
  n = 100, p_M = p_M, p_Y = p_Y, B = 200, errorM = 1, errorY = 1,
  mean_tau = 0.39, n_mc = 50, mag_XM = mag_medium_X, mag_MY = mag_zero_YM,
  magnitude = NULL, alpha_level = 0.05
)
