## Update

This is a minor update for dplyr 0.8.0 compatability, removing a dependency 
on `purrr`.

## Test environments

* local Windows 10 64 install, R 3.5.2
* local OS X, R 3.5.2
* Windows Server 2012 R2 x64 (on appveyor), R 3.5.2
* Ubuntu 14.04 LTS (on travis-ci)
  * R-release (both with and without pandoc)
  * R-devel  (both with and without pandoc)
* OS X 10.13 (on travis-ci), R-release
* win-builder (devel and release)
* local R-devel --enable-long-double=no via docker

## R CMD check results

* There were no NOTEs, ERRORs or WARNINGs.

## Downstream dependencies

There are no downstream dependencies on this package.
