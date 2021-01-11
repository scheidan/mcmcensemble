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
#' @importFrom future.apply future_apply
#' @importFrom progressr progressor
#'
s.m.mcmc <- function(f, lower.inits, upper.inits, max.iter, n.walkers, ...) {

  n.dim <- length(lower.inits)
  ## initial values

  chain.length <- max.iter %/% n.walkers

  p <- progressor(chain.length)

  ensemble.old <- matrix(
    runif(n.dim*n.walkers, lower.inits, upper.inits),
    nrow = n.walkers,
    ncol = n.dim,
    byrow = TRUE
  )
  # This allows utilisation of named vectors in f()
  colnames(ensemble.old) <- names(lower.inits)

  log.p.old <- future_apply(ensemble.old, 1, f, ..., future.seed = TRUE)

  if (!is.vector(log.p.old, mode = "numeric")) {
    stop("Function 'f' should return a numeric of length 1", call. = FALSE)
  }

  log.p <- matrix(NA_real_, nrow = n.walkers, ncol = chain.length)
  samples <- array(NA_real_, dim = c(n.walkers, chain.length, n.dim))

  log.p[, 1] <- log.p.old
  samples[, 1, ] <- ensemble.old

  p()

  ## the loop

  for (l in 2:chain.length) {

    z <- ((runif(n.walkers) + 1)^2) / 2

    a <- vapply(
      seq_len(n.walkers),
      function(n) sample((1:n.walkers)[-n], 1),
      integer(1)
    )

    par.active <- ensemble.old[a, ]

    ensemble.new <- par.active + z * (ensemble.old - par.active)

    log.p.new <- future_apply(ensemble.new, 1, f, ..., future.seed = TRUE)

    val <- z^(n.dim - 1) * exp(log.p.new - log.p.old)

    # We don't want to get rid of Inf values since +Inf is a valid value to
    # accept a change. If we forbid, we are actually forbidding large log.p
    # changes.
    acc <- !is.na(val) & val > runif(n.walkers)

    ensemble.old[acc, ] <- ensemble.new[acc, ]
    log.p.old[acc] <- log.p.new[acc]

    samples[, l, ] <- ensemble.old
    log.p[, l] <- log.p.old

    p()
  }

  return(list(samples = samples, log.p = log.p))

}
