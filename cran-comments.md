## Resubmission
This is a resubmission. In this version I have:

* Setting an environmental variable to run time consuming tests only in
  local and travis builds, not in CRAN checks. Now, win_builder tests and
  examples checks are around 40s (there could be +/- 2 seconds difference
  between release, devel and old release):
  
  ## ** running examples for arch 'i386' ... [37s] OK
  ## ** running examples for arch 'x64' ... [34s] OK
  ## * checking for unstated dependencies in 'tests' ... OK
  ## * checking tests ...
  ## ** running tests for arch 'i386' ... [37s] OK
  ##   Running 'testthat.R' [36s]
  ## ** running tests for arch 'x64' ... [32s] OK
  ##   Running 'testthat.R' [31s]

## Test environments
* local Arch Linux install, R 4.0.2
* ubuntu 16.04 LTS (on travis-ci), R 3.6.3 (oldrelease)
* ubuntu 16.04 LTS (on travis-ci), R 4.0.2 (release)
* ubuntu 16.04 LTS (on travis-ci), R 2020-08-20 r79052 (devel)
* macOS 10.13.6 (on travis-ci), R 3.6.3 (oldrelease)
* macOS 10.13.6 (on travis-ci), R 4.0.2 (release)
* win-builder (oldrelease)
* win-builder (release)
* win-builder (devel)
    
Results for all environments were:

## R CMD check local results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package