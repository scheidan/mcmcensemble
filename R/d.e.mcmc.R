#' MCMC Ensemble sampler with the differential evolution jump move
#'
#' Markov Chain Monte Carlo sampler: using the differential evolution jump move
#' (implementation of the ter Braak differential evolution)
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
#'
#' @importFrom future.apply future_apply
#'
d.e.mcmc <- function(f, lower.inits, upper.inits, max.iter, n.walkers, ...) {

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

    z <- 2.38 / sqrt(2 * n.dim)
    if (l %% 10 == 0) {
      z <- 1
    }

    s <- vapply(
      seq_len(n.walkers),
      function(n) sample((1:n.walkers)[-n], 2, replace = FALSE),
      integer(2)
    )

    par.active.1 <- ensemble.old[s[1, ], ]
    par.active.2 <- ensemble.old[s[2, ], ]

    ensemble.new <- ensemble.old + z * (par.active.1 - par.active.2)

    log.p.new <- future_apply(ensemble.new, 1, f, ..., future.seed = TRUE)

    val <- exp(log.p.new - log.p.old)

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
