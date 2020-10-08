
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MCMC Ensemble Sampler

<!-- badges: start -->

[![R build
status](https://github.com/Bisaloo/MCMCEnsembleSampler/workflows/R-CMD-check/badge.svg)](https://github.com/Bisaloo/MCMCEnsembleSampler/actions)
[![Codecov test
coverage](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler/branch/master/graph/badge.svg)](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler?branch=master)
<!-- badges: end -->

Ensemble Markov Chain Monte Carlo samplers with different strategies to
generate proposals. Either the *stretch move* as proposed by Goodman and
Weare (2010), or a *differential evolution jump move* (similar to ter
Braak and Vrugt, 2008) is used.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("Bisaloo/MCMCEnsembleSampler")
```

## Usage

``` r
library(MCMCEnsembleSampler)

## a log-pdf to sample from
p.log <- function(x) {
    B <- 0.03                              # controls 'bananacity'
    -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
}


## use stretch move
res1 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, method="stretch")
#> Using stretch move with 10 walkers.
str(res1)
#> List of 2
#>  $ samples: num [1:10, 1:300, 1:2] 0.776 0.163 0.15 0.298 0.354 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : chr [1:10] "walker_1" "walker_2" "walker_3" "walker_4" ...
#>   .. ..$ : chr [1:300] "generation_1" "generation_2" "generation_3" "generation_4" ...
#>   .. ..$ : chr [1:2] "a" "b"
#>  $ log.p  : num [1:10, 1:300] -3.94 -3.43 -3.6 -4.46 -2.28 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "walker_1" "walker_2" "walker_3" "walker_4" ...
#>   .. ..$ : chr [1:300] "generation_1" "generation_2" "generation_3" "generation_4" ...
```

If the [coda](https://cran.r-project.org/package=coda) package is
installed, you can then use the `coda = TRUE` argument to get objects of
class `mcmc.list`. The coda package then allows you to call `summary()`
and `plot()` to get informative and nicely formatted results and plots:

``` r
## use stretch move, return samples as 'coda' object
res2 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, method="stretch", coda=TRUE)
#> Using stretch move with 10 walkers.

summary(res2$samples)
#> 
#> Iterations = 1:300
#> Thinning interval = 1 
#> Number of chains = 10 
#> Sample size per chain = 300 
#> 
#> 1. Empirical mean and standard deviation for each variable,
#>    plus standard error of the mean:
#> 
#>     Mean    SD Naive SE Time-series SE
#> a 0.7812 7.363  0.13443         0.8971
#> b 1.2648 2.167  0.03957         0.2474
#> 
#> 2. Quantiles for each variable:
#> 
#>      2.5%     25%   50%   75%  97.5%
#> a -14.146 -4.1775 1.130 5.914 14.729
#> b  -4.005  0.2357 1.744 2.783  4.293
plot(res2$samples)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r

## use different evolution move, return samples as 'coda' object
res3 <- MCMCEnsemble(p.log, lower.inits=c(a=0, b=0), upper.inits=c(a=1, b=1),
                     max.iter=3000, n.walkers=10, 
                     method="differential.evolution", coda=TRUE)
#> Using differential.evolution move with 10 walkers.

summary(res3$samples)
#> 
#> Iterations = 1:300
#> Thinning interval = 1 
#> Number of chains = 10 
#> Sample size per chain = 300 
#> 
#> 1. Empirical mean and standard deviation for each variable,
#>    plus standard error of the mean:
#> 
#>      Mean    SD Naive SE Time-series SE
#> a -1.9650 8.266  0.15092         0.7680
#> b  0.7612 2.592  0.04733         0.2674
#> 
#> 2. Quantiles for each variable:
#> 
#>      2.5%     25%    50%   75%  97.5%
#> a -16.982 -8.0114 -1.857 4.681 11.685
#> b  -5.593 -0.4268  1.350 2.500  4.405
plot(res3$samples)
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

## References

  - ter Braak, C. J. F. and Vrugt, J. A. (2008) Differential Evolution
    Markov Chain with snooker updater and fewer chains. Statistics and
    Computing, 18(4), 435–446,
    <https://doi.org/10.1007/s11222-008-9104-9>
  - Goodman, J. and Weare, J. (2010) Ensemble samplers with affine
    invariance. Communications in Applied Mathematics and Computational
    Science, 5(1), 65–80, <https://doi.org/10.2140/camcos.2010.5.65>

## Similar projects

This package also has (independent) implementations in other languages:

  - emcee in Python (<https://doi.org/10.21105/joss.01864>)
  - gwmcmc in Matlab (<https://github.com/grinsted/gwmcmc>)
