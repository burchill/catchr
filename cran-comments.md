## Test environments
* local OS X install, R 3.3.1
* Ubuntu 14.04.5 LTS (on travis-ci), R versions 3.1, 3.2, 3.3, 3.4, 3.5
* Fedora 28, R 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

    - the name of the package was listed as "Possibly mis-spelled words in DESCRIPTION"
    
    - the example for one function has an elapsed time > 5s because it uses `Sys.sleep()`
    	for a few seconds to demonstrate code functionality. 

## Downstream dependencies
There are currently no downstream dependencies

 