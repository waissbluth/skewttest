

test_that("skwet.test runs", {
  # Generate 100 sample of a gamma distribution
  x <- rgamma(100,2,1)-2
  # Perform a skewed t-test
  result <- skewt.test(x)
  expect_is(result, "htest")
})

test_that("mu works", {
  x <- 0:100
  # Perform a skewed t-test
  result1 <- skewt.test(x-50)
  result2 <- skewt.test(x,mu=50)
  expect_equal(result1$p.value,result2$p.value, tolerance=0.05)
})

