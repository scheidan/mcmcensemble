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
#' @noRd
#'
#' @references
#' Goodman, J. and Weare, J. (2010) Ensemble samplers with affine invariance.
#' Communications in Applied Mathematics and Computational Science, 5(1), 65–80,
#' \doi{10.2140/camcos.2010.5.65}
#'
#' @importFrom future.apply future_apply
#' @importFrom progressr progressor
#'
s.m.mcmc <- function(f, inits, max.iter, n.walkers, ...) {
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

      ## -- random split into two subsets
      perm <- sample.int(n.walkers)
      subset.A <- perm[ seq_len(n.walkers / 2)]
      subset.B <- perm[-seq_len(n.walkers / 2)]

      ## loop over the two halfs:
      for (pass in 1:2) {

          active.idx  <-  if (pass == 1) subset.1 else subset.2
          inactive.idx <- if (pass == 1) subset.2 else subset.1

          ## for each active particle choose a partner from the inactive subset
          partner.idx <- sample(inactive.idx, length(active.idx), replace = TRUE)

          partner <- ensemble.old[partner.idx, , drop = FALSE]
          active  <- ensemble.old[active.idx, , drop = FALSE]

          ##  proposal  x' = y + z·(x − y)
          z <- ((runif(length(active.idx)) + 1)^2) / 2
          prop <- partner + z * (active - partner)

          ## acceptance probability
          log.p.new <- future_apply(prop, 1, f, ..., future.seed = TRUE)

          ## We don't want to get rid of Inf values since +Inf is a valid value to
          ## accept a change. If we forbid, we are actually forbidding large log.p
          ## changes.
          q <- z^(n.params - 1) * exp(log.p.new - log.p.old[active.idx])
          accept  <- !is.na(q) & q > runif(length(q))

          ## -- apply accepted moves
          if (any(accept)) {
              idx.acc                 <- active.idx[accept]
              ensemble.old[idx.acc, ] <- prop[accept, ]
              log.p.old[idx.acc]      <- log.p.new[accept]
          }
      }

      samples[, l, ] <- ensemble.old
      log.p[,  l]  <- log.p.old
      p()
  }

  return(list(samples = samples, log.p = log.p))
}
