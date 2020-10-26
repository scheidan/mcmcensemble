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

  log.p <- matrix(NA_real_, nrow = n.walkers, ncol = chain.length)
  log.p.old <- rep_len(NA_real_, n.walkers)
  log.p.new <- rep_len(NA_real_, n.walkers)
  ensemble.old <- matrix(NA_real_, nrow = n.walkers, ncol = n.dim)
  ensemble.new <- matrix(NA_real_, nrow = n.walkers, ncol = n.dim)
  samples <- array(NA_real_, dim = c(n.walkers, chain.length, n.dim))
  a <- rep_len(NA_integer_, n.walkers)

  ensemble.old[1, ] <- runif(n.dim, lower.inits, upper.inits)
  logres <- f(ensemble.old[1, ], ...)
  if (length(logres) != 1 || !is.numeric(logres)) {
    stop("Function 'f' should return a numeric of length 1", call. = FALSE)
  }
  log.p.old[1] <- logres

  for (k in 2:n.walkers) {
    ensemble.old[k, ] <- runif(n.dim, lower.inits, upper.inits)
    log.p.old[k] <- f(ensemble.old[k, ], ...)
  }

  log.p[, 1] <- log.p.old
  samples[, 1, ] <- ensemble.old

  ## the loop

  for (l in 2:chain.length) {

    z <- ((runif(n.walkers) + 1)^2) / 2

    for (n in 1:n.walkers) {

      a[n] <- sample((1:n.walkers)[-n], 1)

    }

    par.active <- ensemble.old[a, ]

    ensemble.new <- par.active + z * (ensemble.old - par.active)

    for (n in 1:n.walkers) {

      log.p.new[n] <- f(ensemble.new[n, ], ...)

    }

    val <- z^(n.dim - 1) * exp(log.p.new - log.p.old)

    val[!is.finite(val)] <- 0

    acc <- val > runif(n.walkers)

    ensemble.old[acc, ] <- ensemble.new[acc, ]
    log.p.old[acc] <- log.p.new[acc]

    samples[, l, ] <- ensemble.old
    log.p[, l] <- log.p.old
  }

  mcmc.list <- list(samples = samples, log.p = log.p)

  return(mcmc.list)
}
