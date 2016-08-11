# Bootstrapped skewness-adjusted t-test
#
# This is an implementation of the bootstrapped skewness-adjusted t-test
# from Lyon et al (1999). Code based on `getAnywhere(t.test.default)`
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

skewt.test <-
  function (x, mu = 0, conf.level = 0.95, b.frac = 1 / 4, N = 10000, ...)
  {
    if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
      stop("'mu' must be a single number")
    if (!missing(conf.level) &&
        (length(conf.level) != 1 || !is.finite(conf.level) ||
         conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    if (nx < 2)
      stop("not enough 'x' observations")
    stderr <- sqrt(vx / nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    df <- nx - 1
    tstat.calc <-
      function(n,S,gamma)
        sqrt(n) * (S + 1 / 3 * gamma * S ^ 2 + 1 / (6 * n) * gamma)
    S <- (mx - mu) / sqrt(vx)
    gamma <- sum((x - mx) ^ 3) / (nx * sqrt(vx) ^ 3)
    tstat <- tstat.calc(nx,S,gamma)
    nb <- max(nx * b.frac, 3) # Always sample at least 3
    # Calculate N boostrapped statistics, under the null hypothesis
    tsab <- replicate(N,{
      xb <- sample(x,nb,replace = T)
      m_xb <- mean(xb)
      sd_xb <- sd(xb)
      S_b <- (m_xb - mx) / sd_xb
      gamma_b <- sum((xb - m_xb) ^ 3) / (nb * sd_xb ^ 3)
      tstat.calc(length(xb),S_b,gamma_b)
    })
    # Remove NAs from the boostrapped vector
    tsab <- tsab[!is.na(tsab)] # (caused by constant subsamples from x)
    alpha <- 1 - conf.level
    tail <- if (tstat > 0)
      tsab > tstat
    else
      tsab <= tstat
    pval <- min(sum(tail) / (1 + N) * 2, 1)
    cint <- c(quantile(tsab,alpha / 2), quantile(tsab,1 - alpha / 2))
    sd <- sd(tsab)
    dname <- deparse(substitute(x))
    names(tstat) <- "t"
    names(sd) <- "sd"
    names(mu) <- "mean"
    attr(cint, "conf.level") <- conf.level
    method <- "Bootrsapped skewness-adjusted t-test (Lyon et al, 1999)"
    alternative <- "two.sided"
    rval <- list(
      statistic = tstat, parameter = sd, p.value = pval,
      conf.int = cint, estimate = mx, null.value = mu,
      alternative = alternative, method = method, data.name = dname
    )
    class(rval) <- "htest"
    return(rval)
  }
