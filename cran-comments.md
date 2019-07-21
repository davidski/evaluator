## Update

This is a minor hotfix, primarily addressing `vctrs` 0.2.0 compatibility.

## Test environments

* local Windows 10 64 install, R 3.5.3
* local OS X, R 3.6.0
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

The `collector` package (also authored by me) depends on this package. This 
release does not impact collector, apart from fixing CRAN checks on 
collector that fail due to `evaluator`.
