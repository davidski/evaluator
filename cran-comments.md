## Update

This is a minor update, addressing stale links, some deprecated functions 
in dependencies, and internal code maintenance. There are no user-facing changes. 

## Test environments

* local Windows 10 64 install, R 4.0.3
* local OS X, R 4.0.5
* OS X (via GitHub Actions), R-release
* win-builder (R-devel and R-release)
* R-Hub
  * "devian-gcc-devel-nold" - R-devel --enable-long-double=no
  * Windows (R-release and R-devel)
  * Linux (R-release and R-devel)

## R CMD check results

* There were no NOTEs, ERRORs or WARNINGs.
* Some check systems may throw an incorrect dependency error for 
`mc2d`, which was recently updated on CRAN and is not available in 
binaries on all platforms. This error is incorrect and the check 
process works when run on an up to date CI system.

## Downstream dependencies

The `collector` package (also authored by me) depends on this package. 
`collector` is currently archived on CRAN, but a resubmission of this 
package will take place once `evaluator` is updated on CRAN.
