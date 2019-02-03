## Test environments
* local OS X install, R 3.3.1
* Ubuntu 14.04.5 LTS (on travis-ci), R versions 3.1, 3.2, 3.3, 3.4, 3.5
* Fedora 28, R 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE in win-builder:

* checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Zachary Burchill <zach.burchill.code@gmail.com>'

	Possibly mis-spelled words in DESCRIPTION:
  		catchr (15:62)

`catchr` is the name of the package, though.
	

## Downstream dependencies
There are currently no downstream dependencies

 