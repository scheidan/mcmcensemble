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
#' ter Braak, C. J. F. and Vrugt, J. A. (2008) Differential Evolution Markov
#' Chain with snooker updater and fewer chains. Statistics and Computing,
#' 18(4), 435–446, \doi{10.1007/s11222-008-9104-9}
#' .
d.e.mcmc <- function(f, lower.inits, upper.inits, max.iter, n.walkers, ...) {

  n.dim <- length(lower.inits)
  ## initial values

  chain.length <- max.iter %/% n.walkers

  log.p <- matrix(NA_real_, nrow = n.walkers, ncol = chain.length)
  log.p.old <- rep_len(NA_real_, n.walkers)
  ensemble.old <- matrix(NA_real_, nrow = n.walkers, ncol = n.dim)
  samples <- array(NA_real_, dim = c(n.walkers, chain.length, n.dim))
  s <- matrix(NA_integer_, nrow = n.walkers, ncol = 2)

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

    z <- 2.38 / sqrt(2 * n.dim)
    if (l %% 10 == 0) {
      z <- 1
    }

    for (n in 1:n.walkers) {

      # Performance note: it's faster to sample() once and set subset s instead
      # of doing it twice
      s[n, ] <- sample((1:n.walkers)[-n], 2, replace = FALSE)

    }

    par.active.1 <- ensemble.old[s[, 1], ]
    par.active.2 <- ensemble.old[s[, 2], ]

    ensemble.new <- ensemble.old + z * (par.active.1 - par.active.2)

    log.p.new <- apply(ensemble.new, 1, f, ...)

    val <- exp(log.p.new - log.p.old)

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
