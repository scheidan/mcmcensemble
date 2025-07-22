skip_on_cran()
skip_on_ci()
skip_if_not_installed("mvtnorm")

target <- runif(2, min = -50, max = 50)

p.log <- function(x) {
  mvtnorm::dmvnorm(x, target, sigma = diag(2), log = TRUE)
}

unif_inits <- data.frame(
  a = runif(20),
  b = runif(20)
)

test_that("convergence stretch", {
  mcmcres <- MCMCEnsemble(
    p.log,
    inits = unif_inits,
    max.iter = 1e5,
    n.walkers = 20,
    method = "stretch",
    coda = TRUE
  )

  expect_true(
    all(
      vapply(
        coda::HPDinterval(mcmcres$samples, 0.5),
        function(hpd) {
          all(
            target[1] > hpd[1, "lower"],
            target[1] < hpd[1, "upper"],
            target[2] > hpd[2, "lower"],
            target[2] < hpd[2, "upper"]
          )
        },
        logical(1)
      )
    )
  )
})

test_that("convergence differencial evolution", {
  mcmcres <- MCMCEnsemble(
    p.log,
    inits = unif_inits,
    max.iter = 1e5,
    n.walkers = 20,
    method = "d",
    coda = TRUE
  )

  expect_true(
    all(
      vapply(
        coda::HPDinterval(mcmcres$samples, 0.5),
        function(hpd) {
          all(
            target[1] > hpd[1, "lower"],
            target[1] < hpd[1, "upper"],
            target[2] > hpd[2, "lower"],
            target[2] < hpd[2, "upper"]
          )
        },
        logical(1)
      )
    )
  )
})
