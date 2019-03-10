## Resubmission
This is a resubmission, in this version I have:

* Added a brief description of the Sapfluxnet Project and a link to the web
  page of the project in the form <http:sapfluxnet.creaf.cat> as requested.

* Modified doc examples to avoid (when possible) wrapping in \dontrun:

  - All \dontrun blocks have been removed or substituted by \donttest
  
  - All the \donttest blocks that still appear in the examples are beacuse
    one or both of the following reasons:
    
      1. They don't fit in 5 seconds.
      2. They need the user to supply a folder containing the sapfluxnet
         downloaded data.
  
    But nevertheless, these functions had been tested in the test units.
  
  - \dontshow blocks have been added when possible to tests these examples
  - toy examples have been added when possible
  

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

Possibly mis-spelled words in DESCRIPTION:
  Sapfluxnet (2:21, 10:63)


* This is a new release.
* Sapfluxnet is not mis-spelled, is the name of the project.
