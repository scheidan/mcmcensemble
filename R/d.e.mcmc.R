#' MCMC Ensemble sampler with the differential evolution jump move
#'
#' Markov Chain Monte Carlo sampler: using the differential evolution jump move
#' (implementation of the Ter Braak differential evolution)
#'
#' @inheritParams MCMCEnsemble
#'
#' @author Sanda Dejanic
#'
#' @return List containing:
#' - `samples[n.walkers,chain.length,n.dim]`
#' - `log.p[n.walkers,chain.length]`
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @references
#' Braak, C. J. F. ter and Vrugt, J. A. (2008) Differential Evolution Markov
#' Chain with snooker updater and fewer chains. Statistics and Computing,
#' 18(4), 435–446, \doi{10.1007/s11222-008-9104-9}
#' .
d.e.mcmc <- function(f, lower.inits, upper.inits, max.iter, n.walkers, ...) {

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
    for (g in 1:n.dim) {
      ensemble.old[k, g] <- runif(1, lower.inits[g], upper.inits[g])
    }
    log.p.old[k] <- f(ensemble.old[k, ], ...)
  }


  log.p[, 1] <- log.p.old
  samples[, 1, ] <- ensemble.old


  ## the loop

  for (l in 2:chain.length) {
    for (n in 1:n.walkers) {
      z <- 2.38 / sqrt(2 * n.dim)
      if (l %% 10 == 0) {
        z <- 1
      }

      a <- sample((1:n.walkers)[-n], 1)
      b <- sample((1:n.walkers)[-c(n, a)], 1)

      par.active.1 <- ensemble.old[a, ]
      par.active.2 <- ensemble.old[b, ]



      ensemble.new[n, ] <- ensemble.old[n, ] + z * (par.active.1 - par.active.2)

      log.p.new <- f(ensemble.new[n, ], ...)
      if (!is.finite(log.p.new)) {
        acc <- 0
      }
      else {
        acc <- exp(log.p.new - log.p.old[n])
      }
      test <- runif(1)

      if (acc > test) {
        samples[n, l, ] <- ensemble.new[n, ]
        ensemble.old[n, ] <- ensemble.new[n, ]
        log.p[n, l] <- log.p.new
        log.p.old[n] <- log.p.new
      } else {
        samples[n, l, ] <- ensemble.old[n, ]
        log.p[n, l] <- log.p.old[n]
      }
    }
  }

  mcmc.list <- list(samples = samples, log.p = log.p)

  return(mcmc.list)
}
