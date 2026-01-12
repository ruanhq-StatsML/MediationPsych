\name{path_coefficient_generation}
\alias{path_coefficient_generation}
\title{Generate path coefficients by magnitude}
\description{Generates random coefficients around canonical magnitude levels (0, 0.14, 0.39, 0.59).}
\usage{
path_coefficient_generation(m, magnitude_effect, type = 'alpha', err = 0.02)
}
\arguments{
  \item{m}{Number of coefficients.}
  \item{magnitude_effect}{Integer vector of length m with entries in {0,1,2,3}.}
  \item{type}{Label for coefficient type (alpha/beta).}
  \item{err}{Jitter around target magnitude.}
}
\value{A numeric vector of length m.}
\examples{
set.seed(1)
path_coefficient_generation(5, c(0,1,2,3,1))
}
