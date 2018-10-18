## Update

This is a bug fix update, primarily correcting failing test suites recently
picked up by CRAN.

## Test environments

* local Windows 10 64 install, R 3.5.1
* local OS X, R 3.5.1
* Windows Server 2012 R2 x64 (on appveyor), R 3.5.1
* Ubuntu 14.04 LTS (on travis-ci)
  * R-release (both with and without pandoc)
  * R-devel  (both with and without pandoc)
* OS X 10.13 (on travis-ci), R-release
* win-builder (devel and release)
* local R-devel --enable-long-double=no via docker

## R CMD check results

* Win-builder R-Release (not R-Devel) issues a false positive NOTE on a 
  potential mis-spelling.
* There were no ERRORs or WARNINGs.

## Downstream dependencies

There are no downstream dependencies on this package.
