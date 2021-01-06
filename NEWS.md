# mcmcensemble 2.1

## Major changes

* the ensemble sampling can now be parallelized with the future framework. Check
the [README](https://bisaloo.github.io/mcmcensemble/) for more information

## Other user-facing changes

* very large log.p differences between chains do not cause them to be
stuck any more
* addition of a new vignette listing frequently asked questions (with their
answer)

## Dev changes

* new test to make sure the chains converge as expected
* performance improvements

# mcmcensemble 2.0

## Breaking changes

* The argument names and order in `d.e.mcmc()` and `s.m.mcmc()` now match those
of `MCMCEnsemble()`

## Other user-facing changes

* coda package is now only in `Suggests`, instead of being a hard dependency

## Dev changes

* this package is now named mcmcensemble
* roxygen2 documentation now uses markdown syntax
* this package now has unit and regression tests
* various parts of the code have been optimized for speed
