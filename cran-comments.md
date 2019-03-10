## Resubmission
This is a resubmission, in this version I have:

* Added a brief description of the Sapfluxnet Project and a link to the web
  page of the project in the form <http:sapfluxnet.creaf.cat> as requested.

* Modified doc examples to avoid (when possible) wrapping in \dontrun:

  - All \dontrun blocks remaining refers to examples that will fail as they
    need a folder containing the data, provided by the user.
    Nevertheless, these functions have been tested in the test units.
  
  - All the \donttest blocks remaining in the examples are to avoid spend more
    than 5 seconds per .md file. They have been tested with
    R CMD check --run-donttest returning no ERRORs or WARNINGs or NOTEs
    
  - Toy examples have been added when needed or possible
  
  - Examples added to the functions when needed or possible
  

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
