# mediationr2

Utilities for simulating mediation-model data and conducting bootstrap-based power analysis
using R2-type effect sizes (linear models) and deviance pseudo-R2 (generalized linear models).

## Installation (local)

```r
# From the package directory:
# install.packages("devtools")
devtools::install(".")
```

## Quick start

```r
library(mediationr2)

# R2 effect size
n <- 200
X <- rnorm(n)
M <- matrix(rnorm(n * 3), n, 3)
Y <- 0.2 * X + M %*% c(0.1, 0.2, 0.0) + rnorm(n)
r2_es(X, M, Y)

# Z simulation (continuous requires suggested packages)
Zbin <- simulate_Z_cate(n, 5)
```

See `inst/examples/` for more scripts.
