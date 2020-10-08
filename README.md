
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MCMC Ensemble Sampler

<!-- badges: start -->

[![R build
status](https://github.com/Bisaloo/MCMCEnsembleSampler/workflows/R-CMD-check/badge.svg)](https://github.com/Bisaloo/MCMCEnsembleSampler/actions)
[![Codecov test
coverage](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler/branch/master/graph/badge.svg)](https://codecov.io/gh/Bisaloo/MCMCEnsembleSampler?branch=master)
<!-- badges: end -->

Provides ensemble samplers for affine-invariant Monte Carlo Markov
Chain, which allow a faster convergence for badly scaled estimation
problems. Two samplers are proposed: the ‘differential.evolution’
sampler from [ter Braak and Vrugt
(2008)](https://doi.org/10.1007/s11222-008-9104-9) and the ‘stretch’
sampler from [Goodman and Weare
(2010)](https://doi.org/10.2140/camcos.2010.5.65))

## Installation

``` r
# install.packages("remotes")
remotes::install_github("Bisaloo/MCMCEnsembleSampler")
#>      checking for file ‘/tmp/RtmpP8aneS/remotes2350950206382/Bisaloo-MCMCEnsembleSampler-f0d828f/DESCRIPTION’ ...  ✓  checking for file ‘/tmp/RtmpP8aneS/remotes2350950206382/Bisaloo-MCMCEnsembleSampler-f0d828f/DESCRIPTION’
#>   ─  preparing ‘MCMCEnsembleSampler’:
#>    checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  installing the package to process help pages
#>   ─  saving partial Rd database (1.2s)
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘MCMCEnsembleSampler_2.0.tar.gz’
#>      
#> 
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
#>  $ samples: num [1:10, 1:300, 1:2] 0.905 0.748 0.438 0.927 0.943 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : chr [1:10] "walker_1" "walker_2" "walker_3" "walker_4" ...
#>   .. ..$ : chr [1:300] "generation_1" "generation_2" "generation_3" "generation_4" ...
#>   .. ..$ : chr [1:2] "a" "b"
#>  $ log.p  : num [1:10, 1:300] -3.89 -2.23 -3.83 -4.16 -2.55 ...
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
#> a 0.2062 7.423  0.13552         1.1459
#> b 1.3251 2.356  0.04302         0.2813
#> 
#> 2. Quantiles for each variable:
#> 
#>      2.5%     25%   50%   75%  97.5%
#> a -14.522 -4.5107 0.155 5.605 14.178
#> b  -4.221  0.4381 1.852 2.831  4.313
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
#>       Mean    SD Naive SE Time-series SE
#> a -0.07261 8.311   0.1517         0.7856
#> b  0.79816 2.668   0.0487         0.2701
#> 
#> 2. Quantiles for each variable:
#> 
#>      2.5%     25%    50%   75%  97.5%
#> a -14.078 -6.0213 -1.132 5.967 18.003
#> b  -6.281 -0.2003  1.587 2.594  4.081
plot(res3$samples)
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

## Similar projects

The methods used in this package also have (independent) implementations
in other languages:

  - emcee in Python (<https://doi.org/10.21105/joss.01864>)
  - gwmcmc in Matlab (<https://github.com/grinsted/gwmcmc>)
