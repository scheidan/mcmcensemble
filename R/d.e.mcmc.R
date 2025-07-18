#' MCMC Ensemble sampler with the differential evolution jump move
#'
#' Markov Chain Monte Carlo sampler: using the differential evolution jump move
#' (implementation of the ter Braak differential evolution)
#'
#' @inheritParams MCMCEnsemble
#'
#' @author Sanda Dejanic
#'
#' @return Named list containing:
#' - `samples[n.walkers,chain.length,n.params]`
#' - `log.p[n.walkers,chain.length]`
#'
#' @importFrom stats runif
#'
#' @noRd
#'
#' @references
#' ter Braak, C. J. F. and Vrugt, J. A. (2008) Differential Evolution Markov
#' Chain with snooker updater and fewer chains. Statistics and Computing,
#' 18(4), 435–446, \doi{10.1007/s11222-008-9104-9}
#'
#' @importFrom future.apply future_apply
#'
d.e.mcmc <- function(f, inits, max.iter, n.walkers, ...) {
  n.params <- ncol(inits)

  chain.length <- max.iter %/% n.walkers

  p <- progressor(chain.length)

  ensemble.old <- inits

  log.p.old <- future_apply(ensemble.old, 1, f, ..., future.seed = TRUE)

  if (!is.vector(log.p.old, mode = "numeric")) {
    stop("Function 'f' should return a numeric of length 1", call. = FALSE)
  }

  log.p <- matrix(NA_real_, nrow = n.walkers, ncol = chain.length)
  samples <- array(NA_real_, dim = c(n.walkers, chain.length, n.params))

  log.p[, 1] <- log.p.old
  samples[, 1, ] <- ensemble.old

  p()

  for (l in seq_len(chain.length)[-1]) {

      z <- 2.38 / sqrt(2 * n.params)
      if (l %% 10 == 0) z <- 1

      ## -- random permutation and 50 / 50 split
      perm      <- sample.int(n.walkers)
      subset.1  <- perm[seq_len(n.walkers / 2)]
      subset.2  <- perm[-seq_len(n.walkers / 2)]

      ## -- loop over the two halfs:
      for (pass in 1:2) {

          active.idx   <- if (pass == 1) subset.1 else subset.2
          inactive.idx <- if (pass == 1) subset.2 else subset.1

          ## choose two distinct partners for every active walker
          partners <- vapply(
              seq_along(active.idx),
              function(i) sample(inactive.idx, 2, replace = FALSE),
              integer(2)
          )

          partner.1 <- ensemble.old[partners[1, ], , drop = FALSE]
          partner.2 <- ensemble.old[partners[2, ], , drop = FALSE]
          active <-    ensemble.old[active.idx, , drop = FALSE]

          ## proposal  x' = x + z · (y₁ − y₂)
          prop <- active +  z * (partner.1 - partner.2)

          ## acceptance probability
          log.p.new <- future_apply(prop, 1, f, ..., future.seed = TRUE)

          ## We don't want to get rid of Inf values since +Inf is a valid value to
          ## accept a change. If we forbid, we are actually forbidding large log.p
          ## changes.
          q <- exp(log.p.new - log.p.old[active.idx])
          accept <- !is.na(q) & q > runif(length(q))

          ## -- apply accepted moves
          if (any(accept)) {
              idx.acc                 <- active.idx[accept]
              ensemble.old[idx.acc, ] <- prop[accept, ]
              log.p.old[idx.acc]      <- log.p.new[accept]
          }
      }

      ## -- record state and advance progress bar
      samples[, l, ] <- ensemble.old
      log.p[,  l]    <- log.p.old
      p()
  }

  return(list(samples = samples, log.p = log.p))
}
