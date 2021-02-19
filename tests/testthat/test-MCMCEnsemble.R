p.log <- function(x) {
  B <- 0.03
  return(-x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2)
}

test_that("output sizes and names", {

  res1 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                       max.iter=3000, n.walkers=10, method="s")

  expect_type(res1, "list")
  expect_length(res1, 2)
  expect_named(res1, c("samples", "log.p"))

  expect_identical(dim(res1$samples), c(10L, 300L, 2L))
  expect_identical(dim(res1$samples)[1:2], dim(res1$log.p))
  expect_identical(dimnames(res1$samples)[[3]], c("a", "b"))
  expect_identical(attr(res1, "ensemble.sampler"), "stretch")

  res2 <- MCMCEnsemble(p.log, lower.inits=c(0, 0), upper.inits=c(a=1, b=1),
                       max.iter=3000, n.walkers=10, method="s")

  expect_identical(dimnames(res2$samples)[[3]], c("para_1", "para_2"))

})

test_that("coda", {

  skip_if_not_installed("coda")

  res3 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                       max.iter=3000, n.walkers=10, method="d", coda=TRUE)

  expect_s3_class(res3$samples, "mcmc.list")
  expect_identical(attr(res3, "ensemble.sampler"), "differential.evolution")

})

test_that("errors", {
  expect_error(
    MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1),
                 max.iter=3000, n.walkers=10, method="d"),
    "length"
  )

  mockery::stub(MCMCEnsemble, "requireNamespace", FALSE)

  expect_error(
    MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                            max.iter=3000, n.walkers=10, method="d", coda=TRUE),
    "coda"
  )

  fw <- function (x) {
    10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
  }

  expect_error(
    MCMCEnsemble(fw, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                 max.iter=3000, n.walkers=10, method="s"),
    "numeric of length 1"
  )

  expect_error(
    MCMCEnsemble(fw, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                 max.iter=3000, n.walkers=10, method="d"),
    "numeric of length 1"
  )
})

test_that("named arguments", {

  set.seed(20200111)
  res1 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                       max.iter=3000, n.walkers=10, method="s")

  p.log.named <- function(x) {
    B <- 0.03
    return(-x["a"]^2/200 - 1/2*(x["b"]+B*x["a"]^2-100*B)^2)
  }

  set.seed(20200111)
  res2 <- MCMCEnsemble(p.log.named, lower.inits=c(a=0, b=0),
                       upper.inits=c(a=1, b=1), max.iter=3000, n.walkers=10,
                       method="s")

  expect_identical(res1, res2)

})
