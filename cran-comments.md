## Resubmission
This is a resubmission, in this version I have:

* Examples are now executable using tempdir() in those functions which needs to
  write to a folder specified by the user
  
* Test units for those functions also use tempdir() to avoid writing in the user
  home filespace

* 'Sapfluxnet' now appears single quoted in title and description in DESCRIPTION file

## Test environments
* local antergos install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check local results
There were no ERRORs or WARNINGs or NOTEs.

## R CMD check ubuntu (travis-ci) results
There were no ERRORs or WARNINGs or NOTEs.

## R CMD check win-builder results
There were no ERRORs or WARNINGs
There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Victor Granda <victorgrandagarcia@gmail.com>'

New submission

* This is a new release.
