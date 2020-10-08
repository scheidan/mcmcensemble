# MCMC Ensemble Sampler

<!-- badges: start -->
[![R build status](https://github.com/Bisaloo/MCMCEnsembleSampler/workflows/R-CMD-check/badge.svg)](https://github.com/Bisaloo/MCMCEnsembleSampler/actions)
[![Codecov test coverage](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler/branch/master/graph/badge.svg)](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler?branch=master)
<!-- badges: end -->

Ensemble Markov Chain Monte Carlo samplers with different strategies to generate
proposals. Either the _stretch move_ as proposed by Goodman and Weare (2010), or
a _differential evolution jump move_ (similar to Braak and Vrugt, 2008) is used.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Bisaloo/MCMCEnsembleSampler")
```

## Usage

```r
library(MCMCEnsembleSampler)

## a log-pdf to sample from
p.log <- function(x) {
    B <- 0.03                              # controls 'bananacity'
    -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
}

## use stretch move
res1 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, method="stretch")
str(res1)


## use stretch move, return samples as 'coda' object
res2 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, method="stretch", coda=TRUE)

summary(res2$samples)
plot(res2$samples)


## use different evolution move, return samples as 'coda' object
res3 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, 
                     method="differential.evolution", coda=TRUE)

summary(res3$samples)
plot(res3$samples)
```

## References

-  Braak, C. J. F. ter and Vrugt, J. A. (2008) Differential Evolution Markov
 Chain with snooker updater and fewer chains. Statistics and Computing,
 18(4), 435–446, <https://doi.org/10.1007/s11222-008-9104-9>
-  Goodman, J. and Weare, J. (2010) Ensemble samplers with affine invariance.
 Communications in Applied Mathematics and Computational Science, 5(1), 65–80,
 <https://doi.org/10.2140/camcos.2010.5.65>
 
## Similar projects
 
This package also has (independent) implementations in other languages:

- emcee in Python (<https://doi.org/10.21105/joss.01864>)
- gwmcmc in Matlab (<https://github.com/grinsted/gwmcmc>)
