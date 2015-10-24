\name{skewttest}
\alias{skewt.test}
\title{Bootstrapped Skewness-Adjusted t-test}
\usage{
skewt.test(x, ...)

## Default S3 method:
skewt..test(x, mu = 0, conf.level = 0.95, b.frac = 1/4, N = 10000, ...)
}
\description{
Performs one sample bootrsapped skewness adjusted t-test on vectors of data.

Based on `getAnywhere(t.test.default)` and "Improved Methods for Tests of Long-Run Abnormal Stock Returns" by Lyon et al (1999)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}

  \item{mu}{a number indicating the true value of the mean.}

  \item{conf.level}{confidence level of the interval.}

  \item{b.frac}{fraction of the total samples to use in the bootstrap.}

  \item{N}{number of repetitions for the bootsrap.}

  \item{...}{further arguments to be passed to or from methods.}
}
\examples{
# Generate 100 sample of a gamma distribution
x <- rgamma(100,2,1)-2
# Perform a skewed t-test
skewt.test(x)
# Compare to traditional t-test
t.test(x)
}
\references{
Lyon, J. D., Barber, B. M., & Tsai, C.-L. (1999). Improved Methods for Tests of Long-Run Abnormal Stock Returns. {The Journal of Finance, 54(1), 165–201. \url{http://doi.org/10.1111/0022-1082.00101}
}
