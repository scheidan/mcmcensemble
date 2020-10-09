#' MCMC ensemble sampler
#'
#' Ensemble Markov Chain Monte Carlo sampler with different strategies to
#' generate proposals. Either the *stretch move* as proposed by Goodman and
#' Weare (2010), or a *differential evolution jump move* similar to Braak and
#' Vrugt (2008).
#'
#' @param f function that returns a single scalar value proportional to the log
#'   probability density to sample from.
#' @param lower.inits vector specifying for each parameter the lower value the
#'   initial distribution.
#' @param upper.inits vector specifying for each parameter the upper value the
#'   initial distribution.
#' @param max.iter maximum number of function evaluations
#' @param n.walkers number of walkers (ensemble size)
#' @param method method for proposal generation, either `"stretch"`, or
#'   `"differential.evolution"`.
#' @param coda logical. Should the samples be returned as [coda::mcmc.list]
#'   object? (defaults to `FALSE`)
#' @param ... further arguments passed to `f`
#'
#' @return
#' * if `coda = FALSE` a list with:
#' - *samples*: A three dimensional array of samples with dimensions `walker` x
#' `generation` x `parameter`
#' - *log.p*: A matrix with the log density evaluate for each walker at each
#' generation.
#' * if `coda = TRUE` a list with:
#' - *samples*: A object of class [coda::mcmc.list] containing all samples.
#' - *log.p*: A matrix with the log density evaluate for each walker at each
#' generation.
#'
#' @examples
#' ## a log-pdf to sample from
#' p.log <- function(x) {
#'     B <- 0.03                              # controls 'bananacity'
#'     -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
#' }
#'
#' ## use stretch move
#' res1 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
#'                      max.iter=3000, n.walkers=10, method="stretch")
#' str(res1)
#'
#'
#' ## use stretch move, return samples as 'coda' object
#' res2 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
#'                      max.iter=3000, n.walkers=10, method="stretch",
#'                      coda=TRUE)
#'
#' summary(res2$samples)
#' plot(res2$samples)
#'
#'
#' ## use different evolution move, return samples as 'coda' object
#' res3 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
#'                      max.iter=3000, n.walkers=10,
#'                      method="differential.evolution", coda=TRUE)
#'
#' summary(res3$samples)
#' plot(res3$samples)
#'
#' @export
#'
#' @references
#' - ter Braak, C. J. F. and Vrugt, J. A. (2008) Differential Evolution Markov
#' Chain with snooker updater and fewer chains. Statistics and Computing, 18(4),
#' 435–446, \doi{10.1007/s11222-008-9104-9}
#' -  Goodman, J. and Weare, J. (2010) Ensemble samplers with affine invariance.
#' Communications in Applied Mathematics and Computational Science, 5(1), 65–80,
#' \doi{10.2140/camcos.2010.5.65}
#'
MCMCEnsemble <- function(f, lower.inits, upper.inits,
                         max.iter, n.walkers = 10 * length(lower.inits),
                         method = c("stretch", "differential.evolution"),
                         coda = FALSE, ...) {
  if (length(lower.inits) != length(upper.inits)) {
    stop("The length of 'lower.inits' and 'lower.inits' is must be identical!")
  }

  n.dim <- length(lower.inits)
  init.range <- cbind(lower.inits, upper.inits)

  ## run mcmc
  method <- match.arg(method)
  message("Using ", method, " move with ", n.walkers, " walkers.")

  if (method == "differential.evolution") {
    res <- d.e.mcmc(f, lower.inits, upper.inits, max.iter, n.walkers, ...)
  }
  if (method == "stretch") {
    res <- s.m.mcmc(f, lower.inits, upper.inits, max.iter, n.walkers, ...)
  }

  ## add names
  if (is.null(names(lower.inits))) {
    pnames <- paste0("para_", 1:n.dim)
  } else {
    pnames <- names(lower.inits)
  }
  dimnames(res$samples) <- list(
    paste0("walker_", 1:n.walkers),
    paste0("generation_", 1:dim(res$samples)[2]),
    pnames
  )

  dimnames(res$log.p) <- list(
    paste0("walker_", 1:n.walkers),
    paste0("generation_", 1:dim(res$samples)[2])
  )

  ## convert to coda object
  if (coda) {

    if (!requireNamespace("coda", quietly = TRUE)) {
      stop("Package \"coda\" needed for projection plots. Please install it.",
           call. = FALSE
      )
    }

    ll <- lapply(seq_len(n.walkers),
                 function(w) coda::as.mcmc(res$samples[w, , ]))
    res <- list(samples = coda::as.mcmc.list(ll), log.p = res$log.p)
  }

  res
}
