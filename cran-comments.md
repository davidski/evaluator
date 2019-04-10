## Update

This is a major release. Both bug fixes and new features are included. The 
focus is on stabilizing interfaces, preparing for a stable 1.0 milestone.

## Test environments

* local Windows 10 64 install, R 3.5.3
* local OS X, R 3.5.2
* Windows Server 2016 (on appveyor), R 3.5
* Ubuntu 16.04 LTS (on travis-ci)
  * R-release (both with and without pandoc)
  * R-devel  (both with and without pandoc)
* OS X 10.14 (on travis-ci), R-release
* win-builder (devel and release)
* local R-devel --enable-long-double=no via docker

## R CMD check results

* There were no NOTEs, ERRORs or WARNINGs.

## Downstream dependencies

There are no downstream CRAN dependencies on this package.
