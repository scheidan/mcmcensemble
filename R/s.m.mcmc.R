#' MCMC Ensemble sampler with the stretch move (emcee)
#'
#' Markov Chain Monte Carlo sampler: using the stretch move (implementation of
#' the Goodman and Ware emcee)
#'
#' @inheritParams MCMCEnsemble
#'
#' @author Sanda Dejanic
#'
#' @inherit d.e.mcmc return
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @references
#' Goodman, J. and Weare, J. (2010) Ensemble samplers with affine invariance.
#' Communications in Applied Mathematics and Computational Science, 5(1), 65–80,
#' \doi{10.2140/camcos.2010.5.65}
#'
s.m.mcmc <- function(f, lower.inits, upper.inits, max.iter, n.walkers, ...) {

  n.dim <- length(lower.inits)
  ## initial values

  chain.length <- max.iter %/% n.walkers

  log.p <- matrix(NA, nrow = n.walkers, ncol = chain.length)
  log.p.old <- rep(NA, n.walkers)
  ensemble.old <- matrix(NA, nrow = n.walkers, ncol = n.dim)
  ensemble.new <- matrix(NA, nrow = n.walkers, ncol = n.dim)
  samples <- array(NA, dim = c(n.walkers, chain.length, n.dim))
  mcmc.object <- array(NA, dim = c(n.walkers, chain.length, n.dim + 1))

  for (k in 1:n.walkers) {
    ensemble.old[k, ] <- runif(n.dim, lower.inits, upper.inits)
    log.p.old[k] <- f(ensemble.old[k, ], ...)
  }

  log.p[, 1] <- log.p.old
  samples[, 1, ] <- ensemble.old

  ## the loop

  for (l in 2:chain.length) {
    for (n in 1:n.walkers) {
      z <- ((runif(1) + 1)^2) / 2
      a <- sample((1:n.walkers)[-n], 1)
      par.active <- ensemble.old[a, ]

      ensemble.new[n, ] <- par.active + z * (ensemble.old[n, ] - par.active)

      log.p.new <- f(ensemble.new[n, ], ...)
      if (!is.finite(log.p.new)) {
        acc <- 0
      }
      else {
        acc <- z^(n.dim - 1) * exp(log.p.new - log.p.old[n])
      }

      if (acc > runif(1)) {
        ensemble.old[n, ] <- ensemble.new[n, ]
        log.p.old[n] <- log.p.new
      }
      samples[n, l, ] <- ensemble.old[n, ]
      log.p[n, l] <- log.p.old[n]
    }
  }

  mcmc.list <- list(samples = samples, log.p = log.p)

  return(mcmc.list)
}
