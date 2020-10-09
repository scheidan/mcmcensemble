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
