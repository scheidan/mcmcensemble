skip_on_cran()
skip_on_ci()

target <- runif(2, min = -50, max = 50)

p.log <- function(x) {
  mvtnorm::dmvnorm(x, target, sigma = diag(2), log = TRUE)
}

test_that("convergence stretch", {

  tictoc::tic()
  mcmcres <- MCMCEnsemble(
    p.log,
    lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
    max.iter=1e5, n.walkers=10,
    method="stretch",
    coda=TRUE
  )
  tictoc::toc()

  expect_true(
    all(
      vapply(coda::HPDinterval(mcmcres$samples, 0.5), function(hpd) {
        all(
          target[1] > hpd[1, "lower"], target[1] < hpd[1, "upper"],
          target[2] > hpd[2, "lower"], target[2] < hpd[2, "upper"]
        )
      }, logical(1))
    )
  )

})

test_that("convergence differencial evolution", {

  tictoc::tic()
  mcmcres <- MCMCEnsemble(
    p.log,
    lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
    max.iter=1e5, n.walkers=10,
    method="d",
    coda=TRUE
  )
  tictoc::toc()

  expect_true(
    all(
      vapply(coda::HPDinterval(mcmcres$samples, 0.5), function(hpd) {
        all(
          target[1] > hpd[1, "lower"], target[1] < hpd[1, "upper"],
          target[2] > hpd[2, "lower"], target[2] < hpd[2, "upper"]
        )
      }, logical(1))
    )
  )

})
